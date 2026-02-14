use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Mutex};

use crate::{VM, error::RuntimeError, native::NativeFunction, value::RuntimeValue};
use dumpster::sync::Gc;

pub struct HttpRequest;

impl NativeFunction for HttpRequest {
    fn name(&self) -> String {
        String::from("net.http_request_raw")
    }

    fn run(&self, _env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let body = args.pop().unwrap_or(RuntimeValue::Null);
        let url = args.pop().unwrap_or(RuntimeValue::Null);
        let method = args.pop().unwrap_or(RuntimeValue::Null);
        let RuntimeValue::Str(body) = body else {
            return Err(RuntimeError::UnexpectedType(body));
        };
        let RuntimeValue::Str(url) = url else {
            return Err(RuntimeError::UnexpectedType(url));
        };
        let RuntimeValue::Str(method) = method else {
            return Err(RuntimeError::UnexpectedType(method));
        };

        let req = ureq::request(&method, &url).timeout(std::time::Duration::from_secs(5));
        let resp = if body.is_empty() {
            req.call()
        } else {
            req.send_string(&body)
        }
        .map_err(|e| RuntimeError::Io(e.to_string()))?;
        let text = resp
            .into_string()
            .map_err(|e| RuntimeError::Io(e.to_string()))?;
        Ok(RuntimeValue::Str(text))
    }
}

pub struct HttpRequestTry;

impl NativeFunction for HttpRequestTry {
    fn name(&self) -> String {
        String::from("net.http_request_try")
    }

    fn run(&self, _env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let body = args.pop().unwrap_or(RuntimeValue::Null);
        let url = args.pop().unwrap_or(RuntimeValue::Null);
        let method = args.pop().unwrap_or(RuntimeValue::Null);
        let RuntimeValue::Str(body) = body else {
            return Err(RuntimeError::UnexpectedType(body));
        };
        let RuntimeValue::Str(url) = url else {
            return Err(RuntimeError::UnexpectedType(url));
        };
        let RuntimeValue::Str(method) = method else {
            return Err(RuntimeError::UnexpectedType(method));
        };

        let req = ureq::request(&method, &url).timeout(std::time::Duration::from_secs(5));
        let resp = if body.is_empty() {
            req.call()
        } else {
            req.send_string(&body)
        };

        match resp {
            Ok(resp) => {
                let text = resp
                    .into_string()
                    .map_err(|e| RuntimeError::Io(e.to_string()))?;
                Ok(RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Str(text)))))
            }
            Err(e) => Ok(RuntimeValue::Result(Err(Gc::new(RuntimeValue::Str(
                e.to_string(),
            ))))),
        }
    }
}

pub struct TcpConnect;

impl NativeFunction for TcpConnect {
    fn name(&self) -> String {
        String::from("net.tcp_connect")
    }

    fn run(&self, _env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
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

    fn run(&self, _env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
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

    fn run(&self, _env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let listener = args.pop().unwrap_or(RuntimeValue::Null);
        let RuntimeValue::TcpListener(listener) = listener else {
            return Err(RuntimeError::UnexpectedType(listener));
        };
        let (stream, _) = listener.accept().map_err(|e| RuntimeError::Io(e.to_string()))?;
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

    fn run(&self, _env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let len = args.pop().unwrap_or(RuntimeValue::Null);
        let stream = args.pop().unwrap_or(RuntimeValue::Null);
        let RuntimeValue::Int(len) = len else {
            return Err(RuntimeError::UnexpectedType(len));
        };
        let RuntimeValue::TcpStream(stream) = stream else {
            return Err(RuntimeError::UnexpectedType(stream));
        };
        let mut buf = vec![0u8; len.max(0) as usize];
        let mut guard = stream.lock().map_err(|_| RuntimeError::Io("lock".to_string()))?;
        match guard.read(&mut buf) {
            Ok(n) => {
                buf.truncate(n);
                let out = String::from_utf8_lossy(&buf).to_string();
                Ok(RuntimeValue::Str(out))
            }
            Err(e) if e.kind() == std::io::ErrorKind::WouldBlock
                || e.kind() == std::io::ErrorKind::TimedOut
                || e.kind() == std::io::ErrorKind::ConnectionReset =>
            {
                Ok(RuntimeValue::Str(String::new()))
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

    fn run(&self, _env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let data = args.pop().unwrap_or(RuntimeValue::Null);
        let stream = args.pop().unwrap_or(RuntimeValue::Null);
        let RuntimeValue::Str(data) = data else {
            return Err(RuntimeError::UnexpectedType(data));
        };
        let RuntimeValue::TcpStream(stream) = stream else {
            return Err(RuntimeError::UnexpectedType(stream));
        };
        let mut guard = stream.lock().map_err(|_| RuntimeError::Io("lock".to_string()))?;
        let n = guard.write(data.as_bytes()).map_err(|e| RuntimeError::Io(e.to_string()))?;
        Ok(RuntimeValue::Int(n as i64))
    }
}

pub struct TcpClose;

impl NativeFunction for TcpClose {
    fn name(&self) -> String {
        String::from("net.tcp_close")
    }

    fn run(&self, _env: &mut VM, mut args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let _stream = args.pop().unwrap_or(RuntimeValue::Null);
        Ok(RuntimeValue::Null)
    }
}
