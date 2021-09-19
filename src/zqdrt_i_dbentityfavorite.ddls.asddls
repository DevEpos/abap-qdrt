@AbapCatalog.sqlViewName: 'ZQDRTIDBEFAV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'User favorites of DB entities'

/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view ZQDRT_I_DbEntityFavorite
  as select from zqdrt_dbentf
{
  key entity_name          as EntityId,
  key entity_type          as EntityType,
      cast( 'X' as xfeld ) as IsFavorite
}
where
  created_by = $session.user
