# MicroService Prototype
A prototype of a micro-service with a mongodb backend.

## Build
```
cabal build all
```
## Install
```
cabal install user-ms
```

## Run

```
Usage: user-ms (-p|--port PORT) [--logLevel LEVEL] --appName APPNAME
               --collectionName NAME --mongoHost HOST:PORT --mongoUser USERNAME
               --mongoPass PASSWORD --mongoDb DBNAME [--mongoTls]
               [--poolSize POOLSIZE] [--conTimeout TIMEOUT] --sslCert CERTFILE
               --sslKey KEYFILE
  Run Haskell Servant/Wai User Micro-Service

Available options:
  -p,--port PORT           Port number for API server
  --logLevel LEVEL         Logging level (default: Info)
  --appName APPNAME        Application Name
  --collectionName NAME    MongoDB Collection Name
  --mongoHost HOST:PORT    MongoDB hostname and port number
  --mongoUser USERNAME     Mongo user name
  --mongoPass PASSWORD     Mongo password
  --mongoDb DBNAME         Mongo database
  --mongoTls               TLS for mongodb
  --poolSize POOLSIZE      MongoDB Connection Pool Size (default: 5)
  --conTimeout TIMEOUT     Connection pool idle timeout (default: 300s)
  --sslCert CERTFILE       Ssl Certificate file
  --sslKey KEYFILE         Ssl Key file
  -h,--help                Show this help text
```
### Example:

```
user-ms --mongoHost localhost -p 8443 --collectionName testdb --logLevel debug --appName user-ms --mongoUser testuser --mongoPass password --mongoDb testdb --sslCert certificate.pem --sslKey key.pem
```
### Self signed certicate
```
openssl genrsa -out key.pem 2048
openssl req -new -key key.pem -out certificate.csr
openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem
```

Use the `start_mongo.sh` script to create a local mongodb docker instance.

# Projects
|Name|Description|
|----|-----------|
|ms-framework|A micro-service framework using wai and servant|
|user-ms|A sample user micro-service|
|user-client|Sample client app using servant-client|
