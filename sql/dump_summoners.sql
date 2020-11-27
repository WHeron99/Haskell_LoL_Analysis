SELECT json_group_array(json_object(
    'summoner_id', s.id,
    'account_id', s.accountId,
    'puuid', s.puuid,
    'name', s.name,
    'profile_icon_id', s.profileIconId,
    'revision_date', s.revisionDate,
    'summoner_level', s.summonerLevel
))
FROM summoners s