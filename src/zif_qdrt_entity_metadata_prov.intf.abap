"! <p class="shorttext synchronized" lang="en">Metadata provider for DB entity</p>
INTERFACE zif_qdrt_entity_metadata_prov
  PUBLIC .

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Tests if entity exists</p>
    entity_exists
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
        VALUE(result) TYPE REF TO zif_qdrt_field_config,
    "! <p class="shorttext synchronized" lang="en">Returns the metadata of the fields of the entity</p>
    get_fields_metadata
      RETURNING
        VALUE(result) TYPE zif_qdrt_ty_global=>ty_fields_metadata,
    "! <p class="shorttext synchronized" lang="en">Retrieves field metadata</p>
    get_field_metadata
      IMPORTING
        fieldname     TYPE fieldname
        type          TYPE zif_qdrt_ty_global=>ty_field_type DEFAULT zif_qdrt_c_global=>c_field_types-normal_field
      RETURNING
        VALUE(result) TYPE zif_qdrt_ty_global=>ty_field_metadata.
ENDINTERFACE.
