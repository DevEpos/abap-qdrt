"! <p class="shorttext synchronized" lang="en">Provides metadata information</p>
INTERFACE zif_qdrt_metadata_provider
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Retrieves metadata of entity</p>
    get_metadata
      RETURNING
        VALUE(result) TYPE REF TO data,
    "! <p class="shorttext synchronized" lang="en">Tests if entity exists</p>
    entity_exists
      RETURNING
        VALUE(result) TYPE abap_bool.
ENDINTERFACE.
