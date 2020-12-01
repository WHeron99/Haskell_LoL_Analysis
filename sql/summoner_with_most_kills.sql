SELECT pi.summonerName, SUM(p.kills)
FROM match m 
INNER JOIN participant p ON m.gameId = p.gameId
INNER JOIN participantIdentity pi ON pi.gameId = m.gameId AND pi.participantId = p.participantId
GROUP BY pi.summonerName
ORDER BY SUM(p.kills) DESC