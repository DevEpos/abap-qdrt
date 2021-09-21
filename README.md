![](https://img.shields.io/badge/version-WIP-red)
![](https://img.shields.io/badge/ABAP-v7.50+-orange)

# abap-qdrt

ABAP Backend for [UI5 Quick Data Reporter](https://github.com/stockbal/quick-data-reporter)

## Package overview

- **/src**  
  Miscellaneous objects that are needed for the service implementation
- **/src/srv**  
  Contains objects for the REST service

## REST service tests

The folder `/test/service` holds some service tests which can be executed via the VS Code extension [REST Client](https://marketplace.visualstudio.com/items?itemName=humao.rest-client).  
The following settings need to be stored in the VS Code settings for the tests to work

```json
  "rest-client.environmentVariables": {
    "$shared": {
      "sap_dev_server": "<url to abap system>",
      "sap_dev_user": "<user in abap system>",
      "sap_dev_pwd": "<pwd of abap system user>"
    }
  },
```

> NOTE:
> The test can only be executed if the repository was installed on the specified ABAP system

## Installation

Install this repository using [abapGit](https://github.com/larshp/abapGit#abapgit).
