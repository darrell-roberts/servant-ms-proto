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
               --sslCert CERTFILE --sslKey KEYFILE --hashPrefix HASHPREFIX
               --collectionName NAME --mongoHost HOST:PORT --mongoUser USERNAME
               --mongoPass PASSWORD --mongoDb DBNAME [--mongoTls]
               --mongoClientCert CERTFILE --mongoKeyFile KEYFILE
               --mongoCaFile CAFILE [--poolSize POOLSIZE] [--conTimeout TIMEOUT]

  Run Haskell Servant/Wai User Micro-Service

Available options:
  -p,--port PORT           Port number for API server
  --logLevel LEVEL         Logging level (default: Info)
  --appName APPNAME        Application Name
  --sslCert CERTFILE       Ssl Certificate file
  --sslKey KEYFILE         Ssl Key file
  --hashPrefix HASHPREFIX  Hash prefix
  --collectionName NAME    MongoDB Collection Name
  --mongoHost HOST:PORT    MongoDB hostname and port number
  --mongoUser USERNAME     Mongo user name
  --mongoPass PASSWORD     Mongo password
  --mongoDb DBNAME         Mongo database
  --mongoTls               TLS for mongodb
  --mongoClientCert CERTFILE
                           Mongo client certificate file
  --mongoKeyFile KEYFILE   Mongo private key file
  --mongoCaFile CAFILE     mongo CA file
  --poolSize POOLSIZE      MongoDB Connection Pool Size (default: 5)
  --conTimeout TIMEOUT     Connection pool idle timeout (default: 300s)
  -h,--help                Show this help text
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
