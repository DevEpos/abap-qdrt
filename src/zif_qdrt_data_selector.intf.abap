"! <p class="shorttext synchronized" lang="en">Data provider</p>
INTERFACE zif_qdrt_data_selector
  PUBLIC .

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Performs data selection</p>
    select_data
      RETURNING
        VALUE(result) TYPE REF TO data.
ENDINTERFACE.
