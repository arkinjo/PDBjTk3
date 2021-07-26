for i in {1..9}; do
echo $i
psql giraf8_bak<<EOF
UPDATE refaco_nonpolymer${i}
SET ft_3 = ft_9
, ft_4 = ft_10
, ft_5 = ft_11
, ft_9 = ft_3
, ft_10 = ft_4
, ft_11 = ft_5
;
EOF
done