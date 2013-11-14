#####################################################################
###
### This is the 'environment address density'.
### According to the CBS, these densities decide on the degree of
### urbanisation ('stedelijkheid'):
###
###   > 2500 = zeer sterk stedelijk   (extremely urban)
###   > 1500 = sterk stedelijk        (very urban)
###   > 1000 = stedelijk              (urban)
###   >  500 = weinig stedelijk       (barely urban)
###   <  500 = niet stedelijk         (not urban)
###
#####################################################################

dutchInhabitationDensity <- function(twoDigitPostcode, return.as.factor=FALSE) {
  inhabitationDensity <- vector();
  inhabitationDensity[10] <- 6412;
  inhabitationDensity[11] <- 1857;
  inhabitationDensity[12] <- 1831;
  inhabitationDensity[13] <- 1451;
  inhabitationDensity[14] <- 1519;
  inhabitationDensity[15] <- 1679;
  inhabitationDensity[16] <- 1010;
  inhabitationDensity[17] <- 965;
  inhabitationDensity[18] <- 1726;
  inhabitationDensity[19] <- 1706;
  inhabitationDensity[20] <- 2775;
  inhabitationDensity[21] <- 1395;
  inhabitationDensity[22] <- 2121;
  inhabitationDensity[23] <- 2432;
  inhabitationDensity[24] <- 1438;
  inhabitationDensity[25] <- 4891;
  inhabitationDensity[26] <- 1956;
  inhabitationDensity[27] <- 1982;
  inhabitationDensity[28] <- 1693;
  inhabitationDensity[29] <- 1533;
  inhabitationDensity[30] <- 4041;
  inhabitationDensity[31] <- 2432;
  inhabitationDensity[32] <- 1141;
  inhabitationDensity[33] <- 1970;
  inhabitationDensity[34] <- 1282;
  inhabitationDensity[35] <- 3329;
  inhabitationDensity[36] <- 1001;
  inhabitationDensity[37] <- 1175;
  inhabitationDensity[38] <- 1527;
  inhabitationDensity[39] <- 1147;
  inhabitationDensity[40] <- 782;
  inhabitationDensity[41] <- 798;
  inhabitationDensity[42] <- 829;
  inhabitationDensity[43] <- 1101;
  inhabitationDensity[44] <- 672;
  inhabitationDensity[45] <- 559;
  inhabitationDensity[46] <- 1049;
  inhabitationDensity[47] <- 994;
  inhabitationDensity[48] <- 1598;
  inhabitationDensity[49] <- 1149;
  inhabitationDensity[50] <- 2027;
  inhabitationDensity[51] <- 989;
  inhabitationDensity[52] <- 1441;
  inhabitationDensity[53] <- 886;
  inhabitationDensity[54] <- 782;
  inhabitationDensity[55] <- 926;
  inhabitationDensity[56] <- 1766;
  inhabitationDensity[57] <- 1032;
  inhabitationDensity[58] <- 651;
  inhabitationDensity[59] <- 1044;
  inhabitationDensity[60] <- 824;
  inhabitationDensity[61] <- 1010;
  inhabitationDensity[62] <- 1522;
  inhabitationDensity[63] <- 810;
  inhabitationDensity[64] <- 1487;
  inhabitationDensity[65] <- 1783;
  inhabitationDensity[66] <- 715;
  inhabitationDensity[67] <- 1424;
  inhabitationDensity[68] <- 1690;
  inhabitationDensity[69] <- 894;
  inhabitationDensity[70] <- 762;
  inhabitationDensity[71] <- 780;
  inhabitationDensity[72] <- 854;
  inhabitationDensity[73] <- 1464;
  inhabitationDensity[74] <- 1166;
  inhabitationDensity[75] <- 1601;
  inhabitationDensity[76] <- 926;
  inhabitationDensity[77] <- 497;
  inhabitationDensity[78] <- 644;
  inhabitationDensity[79] <- 769;
  inhabitationDensity[80] <- 1267;
  inhabitationDensity[81] <- 538;
  inhabitationDensity[82] <- 1176;
  inhabitationDensity[83] <- 623;
  inhabitationDensity[84] <- 705;
  inhabitationDensity[85] <- 523;
  inhabitationDensity[86] <- 1068;
  inhabitationDensity[87] <- 422;
  inhabitationDensity[88] <- 614;
  inhabitationDensity[89] <- 2268;
  inhabitationDensity[90] <- 337;
  inhabitationDensity[91] <- 366;
  inhabitationDensity[92] <- 674;
  inhabitationDensity[93] <- 600;
  inhabitationDensity[94] <- 790;
  inhabitationDensity[95] <- 508;
  inhabitationDensity[96] <- 703;
  inhabitationDensity[97] <- 2596;
  inhabitationDensity[98] <- 253;
  inhabitationDensity[99] <- 484;
  res <- inhabitationDensity[twoDigitPostcode];
  if (return.as.factor) {
    res <- ifelse(res < 500, 1,
                  ifelse(res < 1000, 2,
                         ifelse(res < 1500, 3,
                                ifelse(res < 2500, 4,
                                       ifelse(res > 2500, 5, NA)))));
    res <- factor(res, levels=c(1:5), labels=c('not urban',
                                               'barely urban',
                                               'urban',
                                               'very urban',
                                               'extremely urban'));
    return(res);
  }
  else {
    return(res);
  }
}