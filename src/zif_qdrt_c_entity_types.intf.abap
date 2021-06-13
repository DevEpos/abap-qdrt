"! <p class="shorttext synchronized" lang="en">Entity types fix values</p>
INTERFACE zif_qdrt_c_entity_types
  PUBLIC.

  CONSTANTS:
    database_table TYPE zif_qdrt_ty_global=>ty_entity_type VALUE 'T',
    cds_view       TYPE zif_qdrt_ty_global=>ty_entity_type VALUE 'C',
    view           TYPE zif_qdrt_ty_global=>ty_entity_type VALUE 'V'.

ENDINTERFACE.
