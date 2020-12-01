SELECT m.gameMode, COUNT(*) 
FROM match m 
GROUP BY m.gameMode 
ORDER BY COUNT(*) DESC