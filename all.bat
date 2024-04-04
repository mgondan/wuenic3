mkdir out
for %%f in (countries\???.pl) do call estimate %%~nf%%~xf
