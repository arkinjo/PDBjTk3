scale=1.5
for e in 0 1 2 3; do
      printf ",GREATEST(1,%.1f*stddev(%s)) :: INTEGER" $scale $env
done
