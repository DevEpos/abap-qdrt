"! <p class="shorttext synchronized" lang="en">Filter provider for CDS Parameter</p>
CLASS zcl_qdrt_cds_parameter_fp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_qdrt_filter_provider.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates new instance of CDS Param filter provider</p>
      constructor
        IMPORTING
          param_filters TYPE zif_qdrt_filter_provider=>ty_filters.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      param_filters TYPE zif_qdrt_filter_provider=>ty_filters.
ENDCLASS.



CLASS zcl_qdrt_cds_parameter_fp IMPLEMENTATION.


  METHOD constructor.
    me->param_filters = param_filters.
    " clear all irrelevant filters
    DELETE me->param_filters WHERE field_name CP |{ zif_qdrt_c_global=>c_param_filter_prefix }*|.
  ENDMETHOD.


  METHOD zif_qdrt_filter_provider~get_filter_string.
    CHECK param_filters IS NOT INITIAL.

    DATA(params) = REDUCE string(
      INIT value = `` sep = ``
      FOR param IN param_filters
      NEXT value = |{ value }{ sep }{ param-field_name } = { cl_abap_dyn_prg=>quote( param-value ) }|
           sep = |, { cl_abap_char_utilities=>cr_lf }    | ).

    result =  VALUE #( ( |( { params } )| ) ).

  ENDMETHOD.


  METHOD zif_qdrt_filter_provider~is_empty.
    result = xsdbool( param_filters IS INITIAL ).
  ENDMETHOD.


ENDCLASS.
