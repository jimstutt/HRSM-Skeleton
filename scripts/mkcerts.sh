# install mkcert, then:
mkcert -install
mkcert localhost
# produces localhost.pem and localhost-key.pem
# use those when serving the directory with a static server that supports TLS
