"! <p class="shorttext synchronized" lang="en">Global constants for Quick Data Reporter API</p>
INTERFACE zif_qdrt_c_global
  PUBLIC.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Prefix for CDS parameter filter</p>
    c_param_filter_prefix TYPE string VALUE '@param:',

    "! <p class="shorttext synchronized" lang="en">Types of fields</p>
    BEGIN OF c_field_types,
      normal_field TYPE zif_qdrt_ty_global=>ty_field_type VALUE '',
      parameter    TYPE zif_qdrt_ty_global=>ty_field_type VALUE 'P',
    END OF c_field_types.

ENDINTERFACE.
