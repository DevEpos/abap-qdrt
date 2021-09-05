"! <p class="shorttext synchronized" lang="en">Metadata provider for DB entity</p>
INTERFACE zif_qdrt_entity_metadata_prov
  PUBLIC .

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Tests if entity exists</p>
    exists
      RETURNING
        VALUE(result) TYPE abap_bool,
    "! <p class="shorttext synchronized" lang="en">Retrieves metadata of entity</p>
    get_metadata
      RETURNING
        VALUE(result) TYPE REF TO data,
    "! <p class="shorttext synchronized" lang="en">Retrieve field configuration of field</p>
    get_field_config
      IMPORTING
        fieldname     TYPE fieldname
      RETURNING
        VALUE(result) TYPE REF TO zif_qdrt_field_config.
ENDINTERFACE.
