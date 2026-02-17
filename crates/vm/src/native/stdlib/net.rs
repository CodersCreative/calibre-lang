use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Mutex};

use crate::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};
use dumpster::sync::Gc;

pub struct HttpRequest;

fn parse_http_args(args: Vec<RuntimeValue>) -> Result<(String, String, String), RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::InvalidFunctionCall);
    }
    let mut parts = Vec::with_capacity(3);
    for value in args {
        let RuntimeValue::Str(s) = value else {
            return Err(RuntimeError::UnexpectedType(value));
        };
        parts.push(s.to_string());
    }

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

    let method = parts[method_idx].clone();
    let url = parts[url_idx].clone();
    let body = parts[body_idx].clone();
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
        let addr = format!("{}:{}", host, port);
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
        let listener = TcpListener::bind(addr).map_err(|e| RuntimeError::Io(e.to_string()))?;
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
