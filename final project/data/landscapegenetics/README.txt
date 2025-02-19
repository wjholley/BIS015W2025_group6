README for Adaptive landscape genetics and malaria across divergent island bird populations
Claire Armstrong, Richard G. Davies, Catalina Gonzalez-Quevedo, Molly Dunne, Lewis G. Spurgin and David S. Richardson

2019-09-10

This readme file describes the data files accompanying the above publication. For any further queries please contact david.richardson@uea.ac.uk

1) TF_PS_pipits.csv
This file contains details on collection, age, malaria strains and genetic data for each sample.
Sample: unique identifier per sample
Year: sample collection year
Island: 'TF' (Tenerife) or 'PS' (Porto Santo)
Age: 'A' (adult) or 'J' (juvenile)
Malaria: presence (1) or absence (0) of avian malaria in blood sample
Presence (Y) or absence (N) of each Plasmodium strain:
	LK6
	LK5
	KYS9
	PS1530
SNP genotype:
	5239s1
	7259s1
	TLR4_1
	TLR4_2
	TLR4_3
	TLR4_4
Presence (1) or absence (0) of SNP allele:
	5239s1_A
	5239s1_T
	7259s1_A
	7259s1_T
	TLR4_1_A
	TLR4_1_G
	TLR4_2_A
	TLR4_2_G
	TLR4_3_C
	TLR4_3_T
	TLR4_4_A
	TLR4_4_C
Presence (1) or absence (0) of TLR4 protein haplotype:
	TLR4_Prot_1
	TLR4_Prot_2
	TLR4_Prot_3
	TLR4_Prot_4
Number of copies of a SNP allele, eg. 5239s1.T = 2 is a homozygote TT:
	5239s1.T
	7259s1.A
	TLR4_1.G
	TLR4_2.A
	TLR4_3.C
	TLR4_4.C
Homozygous (0) or heterozygous (1) SNP:
	5239s1_het
	7259s1_het
	TLR4_1_het
	TLR4_2_het
	TLR4_3_het
	TLR4_4_het
Homozygous (0) or heterozygous (1) TLR4 protein haplotype:
	TLR4_het

2, 3) TF_pipits_2011.csv, PS_pipits_2016.csv
GIS data for samples collected in 2011 on Tenerife/2016 on Porto Santo
Sample: unique identifier per sample
UTM: Universal Transverse Mercator zone
Easting, Northing: Cartesian coordinates of sample
ALTITUDE: altitude (m)
MINTEMP: minimum temperature of the coldest month (°C)
PRECIPITATION: annual precipitation (mm)
DISTWATER: distance to nearest water source
ASPECT: aspect of slope in compass directions
SLOPE: slope (degrees)
DISTFARM: distance to nearest livestock farm
DISTPOUL: distance to nearest poultry farm
DENSITY: pipits/km^2
VEGTYPE: predominant vegetation type
DIST_URB: distance to nearest urban area
autocov1000m: TF_pipits_2011.csv only, autocovariate calculated by Gonzalez-Quevedo et al. (2014)




