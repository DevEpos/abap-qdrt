"! <p class="shorttext synchronized" lang="en">Data provider</p>
INTERFACE zif_qdrt_data_selector
  PUBLIC .

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Performs data selection</p>
    select_data
      IMPORTING
        settings      TYPE zif_qdrt_ty_global=>ty_query_exec_settings
      RETURNING
        VALUE(result) TYPE REF TO data
      RAISING
        zcx_qdrt_selection_common.
ENDINTERFACE.
