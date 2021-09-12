"! <p class="shorttext synchronized" lang="en">Utility for CDS Views</p>
CLASS zcl_qdrt_cds_util DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-DATA:
      "! <p class="shorttext synchronized" lang="en">Range with valid DDL source types</p>
      valid_ddl_source_types TYPE zif_qdrt_ty_global=>ty_ddl_source_type_range READ-ONLY.
    CLASS-METHODS:
      class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_qdrt_cds_util IMPLEMENTATION.


  METHOD class_constructor.
    valid_ddl_source_types = VALUE #(
      sign = 'I' option = 'EQ'
      ( low = ''  )
      ( low = 'V' )
      ( low = 'P' )
      ( low = 'F' )
      ( low = 'W' )
      ( low = 'H' ) ).
  ENDMETHOD.


ENDCLASS.
