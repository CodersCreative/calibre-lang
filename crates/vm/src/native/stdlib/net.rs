use std::collections::HashMap;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream, ToSocketAddrs};
use std::sync::{Arc, Mutex, OnceLock};

use crate::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};
use dumpster::sync::Gc;

fn port_redirects() -> &'static Mutex<HashMap<String, i64>> {
    static REDIRECTS: OnceLock<Mutex<HashMap<String, i64>>> = OnceLock::new();
    REDIRECTS.get_or_init(|| Mutex::new(HashMap::new()))
}

fn key_for(host: &str, port: i64) -> String {
    format!("{host}:{port}")
}

pub struct HttpRequest;

fn parse_http_args(args: Vec<RuntimeValue>) -> Result<(String, String, String), RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::InvalidFunctionCall);
    }
    let [a, b, c]: [RuntimeValue; 3] = args
        .try_into()
        .map_err(|_| RuntimeError::InvalidFunctionCall)?;
    let RuntimeValue::Str(a) = a else {
        return Err(RuntimeError::UnexpectedType(a));
    };
    let RuntimeValue::Str(b) = b else {
        return Err(RuntimeError::UnexpectedType(b));
    };
    let RuntimeValue::Str(c) = c else {
        return Err(RuntimeError::UnexpectedType(c));
    };
    let parts = [a.to_string(), b.to_string(), c.to_string()];

    let looks_like_method = |s: &str| {
        matches!(
            s,
            "GET" | "POST" | "PUT" | "PATCH" | "DELETE" | "HEAD" | "OPTIONS" | "TRACE"
        )
    };
    let looks_like_url = |s: &str| s.contains("://") || s.starts_with("http");

    let mut method_idx = None;
    let mut url_idx = None;
    for (idx, part) in parts.iter().enumerate() {
        if method_idx.is_none() && looks_like_method(part) {
            method_idx = Some(idx);
        }
        if url_idx.is_none() && looks_like_url(part) {
            url_idx = Some(idx);
        }
    }

    let method_idx = method_idx.unwrap_or(0);
    let url_idx = url_idx.unwrap_or_else(|| if method_idx == 0 { 1 } else { 0 });
    let body_idx = (0..3)
        .find(|idx| *idx != method_idx && *idx != url_idx)
        .unwrap_or(2);

    let method = parts[method_idx].to_string();
    let url = parts[url_idx].to_string();
    let body = parts[body_idx].to_string();
    Ok((method, url, body))
}

fn send_http_request(method: &str, url: &str, body: &str) -> Result<String, RuntimeError> {
    let config = ureq::Agent::config_builder()
        .timeout_global(Some(std::time::Duration::from_secs(5)))
        .build();
    let agent = ureq::Agent::new_with_config(config);
    let request = ureq::http::Request::builder()
        .method(method)
        .uri(url)
        .body(body.to_string())
        .map_err(|e| RuntimeError::Io(e.to_string()))?;
    let mut resp = agent
        .run(request)
        .map_err(|e| RuntimeError::Io(e.to_string()))?;
    resp.body_mut()
        .read_to_string()
        .map_err(|e| RuntimeError::Io(e.to_string()))
}

impl NativeFunction for HttpRequest {
    fn name(&self) -> String {
        String::from("net.http_request_raw")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let (method, url, body) = parse_http_args(args)?;
        let text = send_http_request(&method, &url, &body)?;
        Ok(RuntimeValue::Str(std::sync::Arc::new(text)))
    }
}

pub struct HttpRequestTry;

impl NativeFunction for HttpRequestTry {
    fn name(&self) -> String {
        String::from("net.http_request_try")
    }

    fn run(&self, _env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let (method, url, body) = parse_http_args(args)?;
        match send_http_request(&method, &url, &body) {
            Ok(text) => Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Str(
                std::sync::Arc::new(text),
            ))))),
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                std::sync::Arc::new(e.to_string()),
            ))))),
        }
    }
}

pub struct TcpConnect;

impl NativeFunction for TcpConnect {
    fn name(&self) -> String {
        String::from("net.tcp_connect")
    }

    fn run(
        &self,
        _env: &mut VM,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let port = args.pop().unwrap_or(RuntimeValue::Null);
        let host = args.pop().unwrap_or(RuntimeValue::Null);
        let RuntimeValue::Int(port) = port else {
            return Err(RuntimeError::UnexpectedType(port));
        };
        let RuntimeValue::Str(host) = host else {
            return Err(RuntimeError::UnexpectedType(host));
        };
        let remapped_port = {
            let key = key_for(host.as_str(), port);
            port_redirects()
                .lock()
                .ok()
                .and_then(|m| m.get(&key).copied())
                .unwrap_or(port)
        };
        let addr = format!("{}:{}", host, remapped_port);
        let stream = TcpStream::connect(addr).map_err(|e| RuntimeError::Io(e.to_string()))?;
        stream
            .set_nonblocking(false)
            .map_err(|e| RuntimeError::Io(e.to_string()))?;
        let _ = stream.set_read_timeout(Some(std::time::Duration::from_secs(3)));
        let _ = stream.set_write_timeout(Some(std::time::Duration::from_secs(3)));
        Ok(RuntimeValue::TcpStream(Arc::new(Mutex::new(stream))))
    }
}

pub struct TcpListen;

impl NativeFunction for TcpListen {
    fn name(&self) -> String {
        String::from("net.tcp_listen")
    }

    fn run(
        &self,
        _env: &mut VM,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let port = args.pop().unwrap_or(RuntimeValue::Null);
        let host = args.pop().unwrap_or(RuntimeValue::Null);
        let RuntimeValue::Int(port) = port else {
            return Err(RuntimeError::UnexpectedType(port));
        };
        let RuntimeValue::Str(host) = host else {
            return Err(RuntimeError::UnexpectedType(host));
        };
        let addr = format!("{}:{}", host, port);
        let socket_addr = addr
            .to_socket_addrs()
            .map_err(|e| RuntimeError::Io(e.to_string()))?
            .next()
            .ok_or_else(|| RuntimeError::Io("no socket address resolved".to_string()))?;
        let domain = if socket_addr.is_ipv4() {
            socket2::Domain::IPV4
        } else {
            socket2::Domain::IPV6
        };
        let socket =
            socket2::Socket::new(domain, socket2::Type::STREAM, Some(socket2::Protocol::TCP))
                .map_err(|e| RuntimeError::Io(e.to_string()))?;
        socket
            .set_reuse_address(true)
            .map_err(|e| RuntimeError::Io(e.to_string()))?;
        let requested = socket_addr;
        let bind_result = socket.bind(&requested.into());
        if let Err(err) = bind_result {
            if err.kind() == std::io::ErrorKind::AddrInUse {
                let fallback_addr = format!("{}:0", host);
                let fallback = fallback_addr
                    .to_socket_addrs()
                    .map_err(|e| RuntimeError::Io(e.to_string()))?
                    .next()
                    .ok_or_else(|| {
                        RuntimeError::Io("no fallback socket address resolved".to_string())
                    })?;
                socket
                    .bind(&fallback.into())
                    .map_err(|e| RuntimeError::Io(e.to_string()))?;
            } else {
                return Err(RuntimeError::Io(err.to_string()));
            }
        }
        socket
            .listen(128)
            .map_err(|e| RuntimeError::Io(e.to_string()))?;
        let listener: TcpListener = socket.into();
        if let Ok(local_addr) = listener.local_addr()
            && let Ok(mut redirects) = port_redirects().lock()
        {
            let key = key_for(host.as_str(), port);
            if local_addr.port() as i64 != port {
                redirects.insert(key, local_addr.port() as i64);
            } else {
                redirects.remove(&key);
            }
        }
        Ok(RuntimeValue::TcpListener(Arc::new(listener)))
    }
}

pub struct TcpAccept;

impl NativeFunction for TcpAccept {
    fn name(&self) -> String {
        String::from("net.tcp_accept")
    }

    fn run(
        &self,
        _env: &mut VM,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let listener = args.pop().unwrap_or(RuntimeValue::Null);
        let RuntimeValue::TcpListener(listener) = listener else {
            return Err(RuntimeError::UnexpectedType(listener));
        };
        let (stream, _) = listener
            .accept()
            .map_err(|e| RuntimeError::Io(e.to_string()))?;
        let _ = stream.set_read_timeout(Some(std::time::Duration::from_secs(3)));
        let _ = stream.set_write_timeout(Some(std::time::Duration::from_secs(3)));
        Ok(RuntimeValue::TcpStream(Arc::new(Mutex::new(stream))))
    }
}

pub struct TcpRead;

impl NativeFunction for TcpRead {
    fn name(&self) -> String {
        String::from("net.tcp_read")
    }

    fn run(
        &self,
        _env: &mut VM,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let len = args.pop().unwrap_or(RuntimeValue::Null);
        let stream = args.pop().unwrap_or(RuntimeValue::Null);
        let RuntimeValue::Int(len) = len else {
            return Err(RuntimeError::UnexpectedType(len));
        };
        let RuntimeValue::TcpStream(stream) = stream else {
            return Err(RuntimeError::UnexpectedType(stream));
        };
        let mut buf = vec![0u8; len.max(0) as usize];
        let mut guard = stream
            .lock()
            .map_err(|_| RuntimeError::Io("lock".to_string()))?;
        match guard.read(&mut buf) {
            Ok(n) => {
                buf.truncate(n);
                let out = String::from_utf8_lossy(&buf).to_string();
                Ok(RuntimeValue::Str(std::sync::Arc::new(out)))
            }
            Err(e)
                if e.kind() == std::io::ErrorKind::WouldBlock
                    || e.kind() == std::io::ErrorKind::TimedOut
                    || e.kind() == std::io::ErrorKind::ConnectionReset =>
            {
                Ok(RuntimeValue::Str(std::sync::Arc::new(String::new())))
            }
            Err(e) => Err(RuntimeError::Io(e.to_string())),
        }
    }
}

pub struct TcpWrite;

impl NativeFunction for TcpWrite {
    fn name(&self) -> String {
        String::from("net.tcp_write")
    }

    fn run(
        &self,
        _env: &mut VM,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let data = args.pop().unwrap_or(RuntimeValue::Null);
        let stream = args.pop().unwrap_or(RuntimeValue::Null);
        let RuntimeValue::Str(data) = data else {
            return Err(RuntimeError::UnexpectedType(data));
        };
        let RuntimeValue::TcpStream(stream) = stream else {
            return Err(RuntimeError::UnexpectedType(stream));
        };
        let mut guard = stream
            .lock()
            .map_err(|_| RuntimeError::Io("lock".to_string()))?;
        let n = guard
            .write(data.as_bytes())
            .map_err(|e| RuntimeError::Io(e.to_string()))?;
        Ok(RuntimeValue::Int(n as i64))
    }
}

pub struct TcpClose;

impl NativeFunction for TcpClose {
    fn name(&self) -> String {
        String::from("net.tcp_close")
    }

    fn run(
        &self,
        _env: &mut VM,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let _stream = args.pop().unwrap_or(RuntimeValue::Null);
        Ok(RuntimeValue::Null)
    }
}
