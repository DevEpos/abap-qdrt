"! <p class="shorttext synchronized" lang="en">Data selector for CDS view</p>
CLASS zcl_qdrt_cds_data_selector DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_qdrt_data_selector_base
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      zif_qdrt_data_selector.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_qdrt_cds_data_selector IMPLEMENTATION.


  METHOD zif_qdrt_data_selector~select_data.

  ENDMETHOD.


ENDCLASS.
