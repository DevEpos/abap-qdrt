"! <p class="shorttext synchronized" lang="en">Global constants for Quick Data Reporter API</p>
INTERFACE zif_qdrt_c_global
  PUBLIC.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Constants for TADIR object types</p>
    BEGIN OF c_tadir_types,
      search_help TYPE trobjtype VALUE 'SHLP',
    END OF c_tadir_types,

    "! <p class="shorttext synchronized" lang="en">Types of fields</p>
    BEGIN OF c_field_types,
      normal_field TYPE zif_qdrt_ty_global=>ty_field_type VALUE 'normal',
      parameter    TYPE zif_qdrt_ty_global=>ty_field_type VALUE 'param',
    END OF c_field_types,

    "! <p class="shorttext synchronized" lang="en">Constants for custom fieldnames</p>
    BEGIN OF c_special_field_names,
      "! Field used during GROUP BY select to collect aggregation count
      group_count TYPE fieldname VALUE '_GROUP_COUNT',
    END OF c_special_field_names.

ENDINTERFACE.
