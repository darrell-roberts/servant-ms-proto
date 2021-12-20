# MicroService Prototype
A prototype of a micro-service with a mongodb backend.

Build
```
cabal build all
```
Install
```
cabal install user-ms
```

Run:

```
user-ms -h

Usage: user-ms --mongoHost HOST (-p|--port PORT) --collectionName NAME
               [--logLevel LEVEL] --appName APPNAME --mongoUser USERNAME
               --mongoPass PASSWORD --mongoDb DBNAME
  Run Haskell Servant Micro-Service

Available options:
  --mongoHost HOST         MongoDB Host Name
  -p,--port PORT           Port number for API server
  --collectionName NAME    MongoDB Collection Name
  --logLevel LEVEL         Logging level (default: Info)
  --appName APPNAME        Application Name
  --mongoUser USERNAME     Mongo user name
  --mongoPass PASSWORD     Mongo password
  --mongoDb DBNAME         Mongo database
  -h,--help                Show this help text
```

Use the `start_mongo.sh` script to create a local mongodb docker instance.

# Projects
|Name|Description|
|----|-----------|
|ms-framework|A micro-service framework using wai and servant|
|user-ms|A sample user micro-service|
|user-client|Sample client app using servant-client|
