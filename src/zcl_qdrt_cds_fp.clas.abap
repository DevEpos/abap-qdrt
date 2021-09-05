"! <p class="shorttext synchronized" lang="en">Filter provider for CDS View</p>
CLASS zcl_qdrt_cds_fp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      zif_qdrt_filter_provider.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates new instance of CDS Filter provider</p>
      constructor
        IMPORTING
          param_filters TYPE zif_qdrt_filter_provider=>ty_filters.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      param_filters TYPE zif_qdrt_filter_provider=>ty_filters.
ENDCLASS.



CLASS zcl_qdrt_cds_fp IMPLEMENTATION.


  METHOD constructor.
    me->param_filters = param_filters.
    " clear all irrelevant filters
    DELETE me->param_filters WHERE field_name CP |{ zif_qdrt_c_global=>c_param_filter_prefix }*|.
  ENDMETHOD.


  METHOD zif_qdrt_filter_provider~get_filter_string.


  ENDMETHOD.


ENDCLASS.
