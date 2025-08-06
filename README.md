General-purpose server written in Haskell 
# Comprehensive TLS Server

A full-featured, secure web server written in Haskell with TLS encryption, RESTful API endpoints, file serving capabilities, session management, and real-time features.

## 🚀 Features

### Core Functionality
- ✅ **TLS/SSL Encryption** - Secure HTTPS connections
- ✅ **HTTP Protocol Support** - Full HTTP/1.1 implementation
- ✅ **Static File Serving** - Serve files with proper MIME types
- ✅ **RESTful API** - JSON-based API endpoints
- ✅ **File Upload/Download** - Handle file transfers
- ✅ **Session Management** - Track user sessions
- ✅ **Request Logging** - Comprehensive access logs
- ✅ **WebSocket-like Features** - Real-time communication
- ✅ **Directory Listing** - Browse server directories
- ✅ **Multi-threading** - Concurrent client handling

### Security Features
- 🔒 TLS 1.2+ encryption with configurable cipher suites
- 🔐 Session-based authentication
- 🛡️ Request validation and sanitization
- 📝 Comprehensive audit logging

## 📋 Requirements

### System Requirements
- GHC (Glasgow Haskell Compiler) 8.10+
- Cabal or Stack build system
- OpenSSL for certificate generation

### Haskell Dependencies
```haskell
network >= 3.1
tls >= 1.5
x509-system >= 1.6
aeson >= 2.0
stm >= 2.5
http-types >= 0.12
cryptonite >= 0.29
process >= 1.6
directory >= 1.3
time >= 1.9
filepath >= 1.4
containers >= 0.6
text >= 1.2
bytestring >= 0.11
```

## 🛠️ Installation

### 1. Install Dependencies
```bash
# Using Cabal
cabal update
cabal install --dependencies-only

# Using Stack
stack setup
stack build --dependencies-only
```

### 2. Generate TLS Certificates
```bash
# Self-signed certificate (for development)
openssl req -x509 -newkey rsa:4096 -keyout server.key -out server.crt -days 365 -nodes -subj "/C=US/ST=State/L=City/O=Organization/CN=localhost"

# Production certificate (use Let's Encrypt or your CA)
# certbot certonly --standalone -d yourdomain.com
```

### 3. Build and Run
```bash
# Compile
ghc -threaded -rtsopts -with-rtsopts=-N server.hs

# Or run directly
runhaskell server.hs

# With runtime options for better performance
./server +RTS -N4 -RTS
```

## ⚙️ Configuration

The server uses a `ServerConfig` data structure that can be modified in the `main` function:

```haskell
ServerConfig 
{ port = "8443"                    -- Server port
, certFile = "server.crt"          -- TLS certificate path
, keyFile = "server.key"           -- TLS private key path  
, documentRoot = "www"             -- Static files directory
, logFile = "logs/server.log"      -- Log file path
, maxConnections = 100             -- Max concurrent connections
, enableAuth = False               -- Enable authentication
}
```

## 📁 Directory Structure
```
project/
├── server.hs              # Main server code
├── server.crt             # TLS certificate
├── server.key             # TLS private key
├── www/                   # Document root (auto-created)
│   └── index.html         # Default homepage
├── uploads/               # File upload directory (auto-created)
├── logs/                  # Server logs (auto-created)
└── README.md             # This file
```

## 🌐 API Endpoints

### Public Endpoints
| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/` | Main homepage |
| GET | `/dir` | Directory listing |
| GET | `/*` | Static file serving |

### API Endpoints
| Method | Endpoint | Description | Response |
|--------|----------|-------------|----------|
| GET | `/api/status` | Server status | `{"status":"running","version":"1.0"}` |
| GET | `/api/sessions` | Active sessions | Array of session objects |
| GET | `/api/logs` | Request logs (last 100) | Array of log entries |
| GET | `/api/system` | System information | System stats |
| POST | `/api/upload` | File upload | Upload confirmation |

### WebSocket-like Commands
Connect to `/ws` and send commands:
- `PING` → `PONG`
- `ECHO <message>` → `ECHO: <message>`
- `TIME` → Current server time
- `SESSIONS` → Active session count

## 🔧 Usage Examples

### Basic File Serving
```bash
# Start server
./server

# Access in browser
https://localhost:8443/

# Serve a file
curl -k https://localhost:8443/example.html
```

### API Usage
```bash
# Check server status
curl -k https://localhost:8443/api/status

# Upload a file
curl -k -X POST -H "filename: test.txt" --data "Hello World" https://localhost:8443/api/upload

# View logs
curl -k https://localhost:8443/api/logs | jq '.'
```

### WebSocket-like Communication
```bash
# Using netcat with OpenSSL
echo "PING" | openssl s_client -connect localhost:8443 -quiet

# Time query
echo "TIME" | openssl s_client -connect localhost:8443 -quiet
```

## 🏗️ Development

### Adding New Endpoints
1. Modify the `handleClient` function
2. Add pattern matching for your route
3. Implement handler function
4. Update this README

```haskell
-- Example: Add new API endpoint
("GET", "/api/custom") -> do
    let response = object ["message" .= "Custom endpoint"]
    return $ buildResponse 200 "OK" 
        [("Content-Type", "application/json")] 
        (L.toStrict $ encode response)
```

### Custom Authentication
Modify the `authenticateUser` function:

```haskell
authenticateUser :: String -> String -> Bool
authenticateUser username password = 
    -- Implement your auth logic
    checkDatabase username password
```

### Logging Configuration
Logs are stored in STM and can be persisted:

```haskell
-- Add to main function for file logging
forever $ do
    threadDelay (60 * 1000000) -- 1 minute
    logs <- readTVarIO logStore
    appendFile (logFile config) (unlines $ map show logs)
```

## 🧪 Testing

### Unit Tests
```bash
# Test basic connectivity
curl -k -v https://localhost:8443/

# Test API endpoints
curl -k https://localhost:8443/api/status

# Test file upload
echo "test content" > test.txt
curl -k -X POST -H "filename: test.txt" --data-binary @test.txt https://localhost:8443/api/upload
```

### Load Testing
```bash
# Using Apache Bench
ab -n 1000 -c 10 -k https://localhost:8443/

# Using wrk
wrk -t12 -c400 -d30s https://localhost:8443/
```

## 🚀 Production Deployment

### 1. Security Hardening
- Use proper SSL certificates from a trusted CA
- Enable authentication (`enableAuth = True`)
- Configure firewall rules
- Set up reverse proxy (nginx/Apache)
- Use non-privileged user account

### 2. Performance Tuning
```bash
# Compile with optimizations
ghc -O2 -threaded -rtsopts server.hs

# Run with tuned RTS options
./server +RTS -N -A32M -H1G -RTS
```

### 3. Systemd Service
Create `/etc/systemd/system/haskell-server.service`:

```ini
[Unit]
Description=Haskell TLS Server
After=network.target

[Service]
Type=simple
User=haskell-server
WorkingDirectory=/opt/haskell-server
ExecStart=/opt/haskell-server/server +RTS -N -RTS
Restart=always
RestartSec=5

[Install]
WantedBy=multi-user.target
```

### 4. Monitoring
- Set up log rotation for access logs
- Monitor memory usage and connection counts
- Use tools like `htop`, `netstat`, `ss` for monitoring

## 🐛 Troubleshooting

### Common Issues

#### Certificate Errors
```bash
# Regenerate self-signed certificate
openssl req -x509 -newkey rsa:4096 -keyout server.key -out server.crt -days 365 -nodes
```

#### Port Already in Use
```bash
# Find process using port 8443
lsof -i :8443
sudo netstat -tulpn | grep :8443

# Kill process if needed
sudo kill -9 <PID>
```

#### Memory Issues
```bash
# Run with memory profiling
./server +RTS -hc -p -RTS

# Analyze with hp2ps
hp2ps -c server.hp
```

#### Permission Errors
```bash
# Ensure proper file permissions
chmod 600 server.key server.crt
chmod 755 www/ uploads/ logs/
```

### Debug Mode
Enable verbose logging by modifying the `logRequest` function:

```haskell
-- Add debug information
logRequest logStore clientAddr method path status (message ++ " | Headers: " ++ show headers)
```

## 📊 Performance Characteristics

### Benchmarks (approximate)
- **Concurrent Connections**: 100+ (configurable)
- **Request Throughput**: 1000+ req/sec (varies by hardware)
- **Memory Usage**: ~50MB base + ~1KB per connection
- **TLS Handshake**: ~10ms (depends on certificate size)

### Scaling Considerations
- Use multiple server instances behind a load balancer
- Consider database backend for session storage
- Implement connection pooling for external services
- Use CDN for static assets

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Update documentation
6. Submit a pull request

### Code Style
- Follow standard Haskell conventions
- Use `hlint` for style checking
- Add type signatures for all functions
- Document complex functions

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🙏 Acknowledgments

- Built with the excellent Haskell TLS library
- Inspired by various HTTP server implementations
- Thanks to the Haskell community for the amazing ecosystem

## 📞 Support

For issues and questions:
1. Check the troubleshooting section
2. Search existing issues
3. Create a new issue with detailed information
4. Include server logs and configuration

---

**Happy serving! 🚀**
