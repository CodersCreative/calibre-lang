use crate::{
    VM,
    error::RuntimeError,
    native::{
        NativeFunction,
        utils::{expect_num_args, pop_or_null, resolve_host, resolve_int, resolve_str},
    },
    value::RuntimeValue,
};
use dumpster::sync::Gc;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream, ToSocketAddrs};
use std::sync::{Arc, Mutex, OnceLock};
use std::{collections::HashMap, time::Duration};

fn port_redirects() -> &'static Mutex<HashMap<String, i64>> {
    static REDIRECTS: OnceLock<Mutex<HashMap<String, i64>>> = OnceLock::new();
    REDIRECTS.get_or_init(|| Mutex::new(HashMap::new()))
}

fn key_for(host: &str, port: i64) -> String {
    format!("{host}:{port}")
}

pub struct HttpRequest;

fn parse_http_args(
    env: &VM,
    mut args: Vec<RuntimeValue>,
) -> Result<(String, String, String), RuntimeError> {
    expect_num_args(&args, &[3])?;

    let parts = [
        resolve_str(env, &pop_or_null(&mut args))?
            .lock()
            .unwrap()
            .to_string(),
        resolve_str(env, &pop_or_null(&mut args))?
            .lock()
            .unwrap()
            .to_string(),
        resolve_str(env, &pop_or_null(&mut args))?
            .lock()
            .unwrap()
            .to_string(),
    ];

    let looks_like_method = |s: &str| {
        matches!(
            s,
            "GET" | "POST" | "PUT" | "PATCH" | "DELETE" | "HEAD" | "OPTIONS" | "TRACE"
        )
    };
    let looks_like_url = |s: &str| s.contains("://") || s.starts_with("http");

    let mut method_idx = None;
    let mut url_idx = None;
    for (idx, part) in parts.iter().enumerate().rev() {
        if method_idx.is_none() && looks_like_method(part) {
            method_idx = Some(idx);
        }
        if url_idx.is_none() && looks_like_url(part) {
            url_idx = Some(idx);
        }
    }

    let method_idx = method_idx.unwrap_or(2);
    let url_idx = url_idx.unwrap_or_else(|| if method_idx == 2 { 1 } else { 2 });
    let body_idx = (0..3)
        .find(|idx| *idx != method_idx && *idx != url_idx)
        .unwrap_or(0);

    let method = parts[method_idx].to_string();
    let url = parts[url_idx].to_string();
    let body = parts[body_idx].to_string();
    Ok((method, url, body))
}

fn send_http_request(method: &str, url: &str, body: &str) -> Result<String, RuntimeError> {
    let config = ureq::Agent::config_builder()
        .http_status_as_error(false)
        .timeout_global(Some(Duration::from_secs(5)))
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

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let (method, url, body) = parse_http_args(env, args)?;
        let text = send_http_request(&method, &url, &body)?;
        Ok(RuntimeValue::Str(Arc::new(Mutex::new(text))))
    }
}

pub struct HttpRequestTry;

impl NativeFunction for HttpRequestTry {
    fn name(&self) -> String {
        String::from("net.http_request_try")
    }

    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let (method, url, body) = parse_http_args(env, args)?;
        match send_http_request(&method, &url, &body) {
            Ok(text) => Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Str(
                Arc::new(Mutex::new(text)),
            ))))),
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                Arc::new(Mutex::new(e.to_string())),
            ))))),
        }
    }
}

pub struct TcpConnect;

impl NativeFunction for TcpConnect {
    fn name(&self) -> String {
        String::from("net.tcp_connect")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let port = resolve_int(env, &pop_or_null(&mut args))?;
        let host = resolve_str(env, &pop_or_null(&mut args))?
            .lock()
            .unwrap()
            .to_string();

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
        let _ = stream.set_read_timeout(Some(Duration::from_secs(3)));
        let _ = stream.set_write_timeout(Some(Duration::from_secs(3)));
        Ok(RuntimeValue::Host(Arc::new(Mutex::new(stream))))
    }
}

pub struct TcpListen;

impl NativeFunction for TcpListen {
    fn name(&self) -> String {
        String::from("net.tcp_listen")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let port = resolve_int(env, &pop_or_null(&mut args))?;
        let host = resolve_str(env, &pop_or_null(&mut args))?
            .lock()
            .unwrap()
            .to_string();

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
        Ok(RuntimeValue::Host(Arc::new(Mutex::new(listener))))
    }
}

pub struct TcpAccept;

impl NativeFunction for TcpAccept {
    fn name(&self) -> String {
        String::from("net.tcp_accept")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[1])?;

        let listener = resolve_host(env, &pop_or_null(&mut args))?;

        let (stream, _) = listener
            .lock()
            .unwrap()
            .downcast_mut::<TcpListener>()
            .unwrap()
            .accept()
            .map_err(|e| RuntimeError::Io(e.to_string()))?;

        let _ = stream.set_read_timeout(Some(Duration::from_secs(3)));
        let _ = stream.set_write_timeout(Some(Duration::from_secs(3)));

        Ok(RuntimeValue::Host(Arc::new(Mutex::new(stream))))
    }
}

pub struct TcpRead;

impl NativeFunction for TcpRead {
    fn name(&self) -> String {
        String::from("net.tcp_read")
    }

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let len = resolve_int(env, &pop_or_null(&mut args))?;
        let stream = resolve_host(env, &pop_or_null(&mut args))?;

        let mut buf = vec![0u8; len.max(0) as usize];
        let mut guard = stream
            .lock()
            .map_err(|_| RuntimeError::Io("lock".to_string()))?;
        match guard.downcast_mut::<TcpStream>().unwrap().read(&mut buf) {
            Ok(n) => {
                buf.truncate(n);
                let out = String::from_utf8_lossy(&buf).to_string();
                Ok(RuntimeValue::Str(Arc::new(Mutex::new(out))))
            }
            Err(e)
                if e.kind() == std::io::ErrorKind::WouldBlock
                    || e.kind() == std::io::ErrorKind::TimedOut
                    || e.kind() == std::io::ErrorKind::ConnectionReset =>
            {
                Ok(RuntimeValue::Str(Arc::new(Mutex::new(String::new()))))
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

    fn run(&self, env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        expect_num_args(&args, &[2])?;

        let data = resolve_str(env, &pop_or_null(&mut args))?;
        let stream = resolve_host(env, &pop_or_null(&mut args))?;

        let mut guard = stream
            .lock()
            .map_err(|_| RuntimeError::Io("lock".to_string()))?;

        let n = guard
            .downcast_mut::<TcpStream>()
            .unwrap()
            .write(data.lock().unwrap().as_bytes())
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
        expect_num_args(&args, &[1])?;

        let _stream = pop_or_null(&mut args);
        Ok(RuntimeValue::Null)
    }
}
