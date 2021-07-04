@AbapCatalog.sqlViewName: 'ZQDRTIDBV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Database view'

define view ZQDRT_I_DbView
  as select from    dd25l as DbView
    inner join      tadir as Repo on  Repo.obj_name = DbView.viewname
                                  and Repo.pgmid    = 'R3TR'
                                  and Repo.object   = 'VIEW'
    left outer join dd25t as Text on  DbView.viewname = Text.viewname
                                  and Text.ddlanguage = $session.system_language
{
  key DbView.viewname as ViewName,
      Text.ddtext     as Description,
      author          as CreatedBy,
      Repo.created_on as CreatedDate,
      as4date         as ChangedDate,
      as4user         as ChangedBy,
      devclass        as DevelopmentPackage,
      Repo.masterlang as OriginalLanguage,
      'V'             as Type
}
where
       DbView.as4local  = 'A'
  and  Repo.genflag     = #genflag.' '
  and(
       DbView.viewclass = #viewclass.' '
    or DbView.viewclass = #viewclass.'D'
  )
