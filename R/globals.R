# Copyright (c) 2021 Genome Research Ltd
#
# Author: CASM/Cancer IT <cgphelp@sanger.ac.uk>
#
# This file is part of RCRISPR.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# 1. The usage of a range of years within a copyright statement contained within
# this distribution should be interpreted as being equivalent to a list of years
# including the first and last year specified and all consecutive years between
# them. For example, a copyright statement that reads ‘Copyright (c) 2005, 2007-
# 2009, 2011-2012’ should be interpreted as being identical to a statement that
# reads ‘Copyright (c) 2005, 2007, 2008, 2009, 2011, 2012’ and a copyright
# statement that reads ‘Copyright (c) 2005-2012’ should be interpreted as being
# identical to a statement that reads ‘Copyright (c) 2005, 2006, 2007, 2008,
# 2009, 2010, 2011, 2012’.
#
utils::globalVariables(c("sgRNA", "gene", "Gene", 'sgrna',
                         "filename", "label", "id_col", "gene_col",
                         "sgrna_lfc", "spread", "gather", "lfc",
                         "plasmid","control","treatment",
                         "group","reads","chr",
                         "slot<-", ".",
                         "data", "end", "filter_mean", "filter_median", "start",
                         "neg.fdr", 'neg.goodsgrna', 'neg.lfc',
                         "pos.fdr", 'pos.goodsgrna', 'pos.lfc',
                         "LFC", "FDR", "is_enriched", "is_depleted",
                         "prop_mapped_reads", "pct_low_sgrnas", "pct_zero_sgrnas",
                         "low_counts_per_sample", ":=", "total_reads", "total_counts",
                         "pct_mapped_reads", "unmapped", "pct_unmapped_reads",
                         "category", "num_reads", "df", "data", "classification",
                         "mean", "values", "median", "val", "ess", "median", "n_essential",
                         "total_essential", "n_nonessential", "total_essential", "n_nonessential",
                         "total_nonessential", "prop_bagel_essential", "setNames", "sd", "NNMD",
                         "Essential.Glass.Delta"))





