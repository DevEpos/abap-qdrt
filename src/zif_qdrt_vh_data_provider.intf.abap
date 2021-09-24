"! <p class="shorttext synchronized" lang="en">Value Help Data provider</p>
INTERFACE zif_qdrt_vh_data_provider
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Retrieves vh data</p>
    get_data
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_qdrt_appl_error.

ENDINTERFACE.
