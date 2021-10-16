# For less output use:
# -dsuppress-all
# -dsuppress-idinfo

stack ghc -- Main.hs -dsuppress-timestamps -dsuppress-all -ddump-to-file -ddump-parsed -ddump-parsed-ast -ddump-rn -ddump-rn-ast -ddump-tc -ddump-tc-ast -ddump-ds-preopt -ddump-simpl-iterations -ddump-simpl-stats -ddump-rule-rewrites -ddump-stg -ddump-cmm-raw -ddump-asm-native 

rm Main.hi Main.o
