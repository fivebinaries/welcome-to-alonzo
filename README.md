# welcome-to-alonzo

Simple smart contacts that greets people into the Alonzo Era.

It also happens to be the first smart contract ever executed on the Cardano maininet.

## Validation

```
Datum value: {"bytes":"57656c636f6d6520746f20416c6f6e7a6f2045726121202d20466976652042696e6172696573207465616d"}
Validator hash: 4f590a3d80ae0312bad0b64d540c3ff5080e77250e9dbf5011630016
```

Out from the cardano-db-sync:

```
csyncdb=> select tx_id,hash from script where type = 'plutus' order by tx_id asc limit 1;
  tx_id   |                            hash                            
----------+------------------------------------------------------------
 14505465 | \x4f590a3d80ae0312bad0b64d540c3ff5080e77250e9dbf5011630016
(1 row)

csyncdb=> 
```

## The `datum` message

```
csyncdb=> select * from datum order by tx_id asc limit 2;
 id |                                hash                                |  tx_id   |                                                value                                                
----+--------------------------------------------------------------------+----------+-----------------------------------------------------------------------------------------------------
 12 | \x5a595ce795815e81d22a1a522cf3987d546dc5bb016de61b002edd63a5413ec4 | 12934527 | {"bytes": "3c33"}
 13 | \x818ee3db3bbbd04f9f2ce21778cac3ac605802a4fcb00c8b3a58ee2dafc17d46 | 12934527 | {"bytes": "57656c636f6d6520746f20416c6f6e7a6f2045726121202d20466976652042696e6172696573207465616d"}
(2 rows)

csyncdb=> 
```

The message:

``` 
$ echo "57656c636f6d6520746f20416c6f6e7a6f2045726121202d20466976652042696e6172696573207465616d" |  xxd -r -p
Welcome to Alonzo Era! - Five Binaries team
$ echo "3c33" |  xxd -r -p
<3
$
```
