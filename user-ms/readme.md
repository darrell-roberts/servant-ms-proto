# A sample user micro-service
This project builds a client library and stand alone user microservice.

## Modules
|Name|Description|
|----|-----------|
|UserService.Client|Functions for an external client using servant-client|
|UserService.Server|The type definitions for the server routes using servant/servant-server|
|UserService.Handlers|The handler functions for each endpoint|
|UserService.Persistence|The mongodb persistence functions used by the handler functions|
|UserService.Types|The data types for the user-ms service|
|Main|The main module that starts the wai/servant service|