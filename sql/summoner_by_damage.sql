SELECT pi.summonerName, AVG(p.totalDamageDealt), AVG(p.totalDamageTaken)
FROM participant p
INNER JOIN participantIdentity pi ON p.gameId = pi.gameId AND p.participantId = pi.participantId
GROUP BY pi.summonerName
HAVING AVG (p.totalDamageDealt) > 0.0 AND AVG(p.totalDamageTaken) > 0.0
ORDER BY AVG (p.totalDamageDealt) DESC, AVG(p.totalDamageTaken) DESC