"! <p class="shorttext synchronized" lang="en">Data Preview provider for DB entities</p>
CLASS zcl_qdrt_entity_data_prev_prov DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates new instance</p>
      constructor
        IMPORTING
          name TYPE zif_qdrt_ty_global=>ty_entity_name
          type TYPE zif_qdrt_ty_global=>ty_entity_type,

      "! <p class="shorttext synchronized" lang="en">Retrieves data</p>
      get_data
        RETURNING
          VALUE(result) TYPE REF TO data.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      entity_name TYPE zif_qdrt_ty_global=>ty_entity_name,
      entity_type TYPE zif_qdrt_ty_global=>ty_entity_type.
ENDCLASS.



CLASS zcl_qdrt_entity_data_prev_prov IMPLEMENTATION.


  METHOD constructor.
    entity_name = name.
    entity_type = type.
  ENDMETHOD.


  METHOD get_data.

  ENDMETHOD.

ENDCLASS.
