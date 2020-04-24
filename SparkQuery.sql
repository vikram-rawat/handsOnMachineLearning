-- !preview conn=spark

SELECT
  cyl,
  mean(hp)
FROM
  `cached_cars`
group by
  cyl
LIMIT
  10