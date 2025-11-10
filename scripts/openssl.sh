mkdir -p certs
openssl req -x509 -newkey rsa:4096 -keyout certs/key.pem -out certs/cert.pem -days 365 -nodes -subj "/CN=localhost"

# Serve static with simple python HTTPS server
python3 -m http.server 3000 --directory logistics-client/client-dist --bind 127.0.0.1 --cgi
# For HTTPS you can use a small node static server that supports TLS or use `caddy`.
