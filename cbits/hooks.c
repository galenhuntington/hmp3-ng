/* specifying -G1 gives us constant heap usage over many hours, without
 * this the heap slowly grows, doubling in 10 hours on my box */
char *ghc_rts_opts = "-G1";
