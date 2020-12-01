SELECT json_group_array(json_object(
	'game_id', m.gameId,
	'game_creation', m.gameCreation,
   'game_duration', m.gameDuration,
   'game_mode', m.gameMode,
   'game_type', m.gameType,
	'teams', (SELECT json_group_array(json_object(
		'team_id', t.teamId,
      'win', t.win,
      'first_blood', t.firstBlood,
      'first_tower', t.firstTower,
      'tower_kills', t.towerKills,
      'inhibitor_kills', t.inhibitorKills,
      'baron_kills', t.baronKills,
      'dragon_kills', t.dragonKills,
      'rift_herald_kills', t.riftHeraldKills
		)) 
      FROM team t 
      WHERE t.gameId = m.gameId),
   'participants', (SELECT json_group_array(json_object(
      'particpant_id', p.participantId,
      'team_id', p.teamId,
      'win', p.win,
      'champion_id', p.championId,
      'kills', p.kills,
      'deaths', p.deaths,
      'assists', p.assists,
      'largest_killing_spree', p.largestKillingSpree,
      'largest_multikill', p.largestMultiKill,
      'total_damage_dealt', p.totalDamageDealt,
      'total_damage_dealt_to_champions', p.totalDamageDealtToChampions,
      'total_damage_taken', p.totalDamageTaken,
      'gold_earned', p.goldEarned,
      'gold_spent', p.goldSpent,
      'total_minions_killed', p.totalMinionsKilled,
      'identity', json_object(
         'account_id', pi.accountId,
         'summoner_name', pi.summonerName,
         'summoner_id', pi.summonerId,
         'current_account_id', pi.currentAccountId
         )
      ))
      FROM participant p 
      INNER JOIN participantIdentity pi ON pi.participantId = p.participantId AND pi.gameId = p.gameId
      WHERE p.gameId = m.gameId)
   )) 
FROM match m