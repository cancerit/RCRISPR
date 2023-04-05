# can plot correlation

    Code
      p$data
    Output
                            sgRNA gene HELA_T0 HELA_T15A_CTRL HELA_T15B_CTRL
      1 A1BG_CACCTTCGAGCTGCTGCGCG A1BG     478            519            439
      2 A1BG_AAGAGCGCCTCGGTCCCAGC A1BG       0              0              0
      3 A1BG_TGGACTTCCAGCTACGGCGC A1BG     274            193            163
        HELA_T15C_CTRL HELA_T15A_OLA HELA_T15B_OLA HELA_T15C_OLA
      1            587           389           591           274
      2             52             0            16             0
      3            161            80           393            45

---

    Code
      p$gg
    Output
      $theme
      List of 97
       $ line                      :List of 6
        ..$ colour       : chr "black"
        ..$ linewidth    : num 0.5
        ..$ linetype     : num 1
        ..$ lineend      : chr "butt"
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ rect                      :List of 5
        ..$ fill         : chr "white"
        ..$ colour       : chr "black"
        ..$ linewidth    : num 0.5
        ..$ linetype     : num 1
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ text                      :List of 11
        ..$ family       : chr ""
        ..$ face         : chr "plain"
        ..$ colour       : chr "black"
        ..$ size         : num 11
        ..$ hjust        : num 0.5
        ..$ vjust        : num 0.5
        ..$ angle        : num 0
        ..$ lineheight   : num 0.9
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ title                     : NULL
       $ aspect.ratio              : NULL
       $ axis.title                : NULL
       $ axis.title.x              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 2.75points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.x.top          :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 2.75points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.x.bottom       : NULL
       $ axis.title.y              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : num 90
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 2.75points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.y.left         : NULL
       $ axis.title.y.right        :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : num -90
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.75points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text                 :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : chr "grey30"
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x               :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 1
        ..$ vjust        : num 0.5
        ..$ angle        : num 90
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 2.2points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x.top           :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 2.2points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x.bottom        : NULL
       $ axis.text.y               :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 1
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 2.2points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.y.left          : NULL
       $ axis.text.y.right         :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.2points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.ticks                :List of 6
        ..$ colour       : chr "grey70"
        ..$ linewidth    : 'rel' num 0.5
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ axis.ticks.x              : NULL
       $ axis.ticks.x.top          : NULL
       $ axis.ticks.x.bottom       : NULL
       $ axis.ticks.y              : NULL
       $ axis.ticks.y.left         : NULL
       $ axis.ticks.y.right        : NULL
       $ axis.ticks.length         : 'simpleUnit' num 2.75points
        ..- attr(*, "unit")= int 8
       $ axis.ticks.length.x       : NULL
       $ axis.ticks.length.x.top   : NULL
       $ axis.ticks.length.x.bottom: NULL
       $ axis.ticks.length.y       : NULL
       $ axis.ticks.length.y.left  : NULL
       $ axis.ticks.length.y.right : NULL
       $ axis.line                 : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ axis.line.x               : NULL
       $ axis.line.x.top           : NULL
       $ axis.line.x.bottom        : NULL
       $ axis.line.y               : NULL
       $ axis.line.y.left          : NULL
       $ axis.line.y.right         : NULL
       $ legend.background         :List of 5
        ..$ fill         : NULL
        ..$ colour       : logi NA
        ..$ linewidth    : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ legend.margin             : 'margin' num [1:4] 5.5points 5.5points 5.5points 5.5points
        ..- attr(*, "unit")= int 8
       $ legend.spacing            : 'simpleUnit' num 11points
        ..- attr(*, "unit")= int 8
       $ legend.spacing.x          : NULL
       $ legend.spacing.y          : NULL
       $ legend.key                :List of 5
        ..$ fill         : chr "white"
        ..$ colour       : logi NA
        ..$ linewidth    : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ legend.key.size           : 'simpleUnit' num 1.2lines
        ..- attr(*, "unit")= int 3
       $ legend.key.height         : NULL
       $ legend.key.width          : NULL
       $ legend.text               :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ legend.text.align         : NULL
       $ legend.title              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ legend.title.align        : NULL
       $ legend.position           : chr "right"
       $ legend.direction          : NULL
       $ legend.justification      : chr "center"
       $ legend.box                : NULL
       $ legend.box.just           : NULL
       $ legend.box.margin         : 'margin' num [1:4] 0cm 0cm 0cm 0cm
        ..- attr(*, "unit")= int 1
       $ legend.box.background     : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ legend.box.spacing        : 'simpleUnit' num 11points
        ..- attr(*, "unit")= int 8
       $ panel.background          :List of 5
        ..$ fill         : chr "white"
        ..$ colour       : logi NA
        ..$ linewidth    : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ panel.border              :List of 5
        ..$ fill         : logi NA
        ..$ colour       : chr "grey70"
        ..$ linewidth    : 'rel' num 1
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ panel.spacing             : 'simpleUnit' num 5.5points
        ..- attr(*, "unit")= int 8
       $ panel.spacing.x           : NULL
       $ panel.spacing.y           : NULL
       $ panel.grid                :List of 6
        ..$ colour       : chr "grey87"
        ..$ linewidth    : NULL
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ panel.grid.major          :List of 6
        ..$ colour       : NULL
        ..$ linewidth    : 'rel' num 0.5
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ panel.grid.minor          :List of 6
        ..$ colour       : NULL
        ..$ linewidth    : 'rel' num 0.25
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ panel.grid.major.x        : NULL
       $ panel.grid.major.y        : NULL
       $ panel.grid.minor.x        : NULL
       $ panel.grid.minor.y        : NULL
       $ panel.ontop               : logi FALSE
       $ plot.background           :List of 5
        ..$ fill         : NULL
        ..$ colour       : chr "white"
        ..$ linewidth    : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ plot.title                :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 1.2
        ..$ hjust        : num 0
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 5.5points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.title.position       : chr "panel"
       $ plot.subtitle             :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 5.5points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.caption              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : num 1
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 5.5points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.caption.position     : chr "panel"
       $ plot.tag                  :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 1.2
        ..$ hjust        : num 0.5
        ..$ vjust        : num 0.5
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.tag.position         : chr "topleft"
       $ plot.margin               : 'margin' num [1:4] 5.5points 5.5points 5.5points 5.5points
        ..- attr(*, "unit")= int 8
       $ strip.background          : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ strip.background.x        : NULL
       $ strip.background.y        : NULL
       $ strip.clip                : chr "inherit"
       $ strip.placement           : chr "inside"
       $ strip.text                :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : chr "white"
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 4.4points 4.4points 4.4points 4.4points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ strip.text.x              :List of 11
        ..$ family       : NULL
        ..$ face         : chr "bold"
        ..$ colour       : chr "black"
        ..$ size         : num 10
        ..$ hjust        : num 0
        ..$ vjust        : NULL
        ..$ angle        : num 90
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ strip.text.x.bottom       : NULL
       $ strip.text.x.top          : NULL
       $ strip.text.y              :List of 11
        ..$ family       : NULL
        ..$ face         : chr "bold"
        ..$ colour       : chr "black"
        ..$ size         : num 10
        ..$ hjust        : num 0
        ..$ vjust        : NULL
        ..$ angle        : num 0
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ strip.text.y.left         :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : num 90
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ strip.text.y.right        : NULL
       $ strip.switch.pad.grid     : 'simpleUnit' num 2.75points
        ..- attr(*, "unit")= int 8
       $ strip.switch.pad.wrap     : 'simpleUnit' num 2.75points
        ..- attr(*, "unit")= int 8
       - attr(*, "class")= chr [1:2] "theme" "gg"
       - attr(*, "complete")= logi TRUE
       - attr(*, "validate")= logi TRUE
      

# can plot correlation with groups

    Code
      p$data
    Output
        x y  g
      1 1 1 g1
      2 2 2 g1
      3 3 3 g1
      4 4 4 g1
      5 5 5 g2
      6 6 6 g2
      7 7 7 g2
      8 8 8 g2

---

    Code
      p$gg
    Output
      $theme
      List of 97
       $ line                      :List of 6
        ..$ colour       : chr "black"
        ..$ linewidth    : num 0.5
        ..$ linetype     : num 1
        ..$ lineend      : chr "butt"
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ rect                      :List of 5
        ..$ fill         : chr "white"
        ..$ colour       : chr "black"
        ..$ linewidth    : num 0.5
        ..$ linetype     : num 1
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ text                      :List of 11
        ..$ family       : chr ""
        ..$ face         : chr "plain"
        ..$ colour       : chr "black"
        ..$ size         : num 11
        ..$ hjust        : num 0.5
        ..$ vjust        : num 0.5
        ..$ angle        : num 0
        ..$ lineheight   : num 0.9
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ title                     : NULL
       $ aspect.ratio              : NULL
       $ axis.title                : NULL
       $ axis.title.x              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 2.75points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.x.top          :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 2.75points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.x.bottom       : NULL
       $ axis.title.y              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : num 90
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 2.75points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.y.left         : NULL
       $ axis.title.y.right        :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : num -90
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.75points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text                 :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : chr "grey30"
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x               :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 1
        ..$ vjust        : num 0.5
        ..$ angle        : num 90
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 2.2points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x.top           :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 2.2points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x.bottom        : NULL
       $ axis.text.y               :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 1
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 2.2points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.y.left          : NULL
       $ axis.text.y.right         :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.2points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.ticks                :List of 6
        ..$ colour       : chr "grey70"
        ..$ linewidth    : 'rel' num 0.5
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ axis.ticks.x              : NULL
       $ axis.ticks.x.top          : NULL
       $ axis.ticks.x.bottom       : NULL
       $ axis.ticks.y              : NULL
       $ axis.ticks.y.left         : NULL
       $ axis.ticks.y.right        : NULL
       $ axis.ticks.length         : 'simpleUnit' num 2.75points
        ..- attr(*, "unit")= int 8
       $ axis.ticks.length.x       : NULL
       $ axis.ticks.length.x.top   : NULL
       $ axis.ticks.length.x.bottom: NULL
       $ axis.ticks.length.y       : NULL
       $ axis.ticks.length.y.left  : NULL
       $ axis.ticks.length.y.right : NULL
       $ axis.line                 : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ axis.line.x               : NULL
       $ axis.line.x.top           : NULL
       $ axis.line.x.bottom        : NULL
       $ axis.line.y               : NULL
       $ axis.line.y.left          : NULL
       $ axis.line.y.right         : NULL
       $ legend.background         :List of 5
        ..$ fill         : NULL
        ..$ colour       : logi NA
        ..$ linewidth    : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ legend.margin             : 'margin' num [1:4] 5.5points 5.5points 5.5points 5.5points
        ..- attr(*, "unit")= int 8
       $ legend.spacing            : 'simpleUnit' num 11points
        ..- attr(*, "unit")= int 8
       $ legend.spacing.x          : NULL
       $ legend.spacing.y          : NULL
       $ legend.key                :List of 5
        ..$ fill         : chr "white"
        ..$ colour       : logi NA
        ..$ linewidth    : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ legend.key.size           : 'simpleUnit' num 1.2lines
        ..- attr(*, "unit")= int 3
       $ legend.key.height         : NULL
       $ legend.key.width          : NULL
       $ legend.text               :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ legend.text.align         : NULL
       $ legend.title              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ legend.title.align        : NULL
       $ legend.position           : chr "right"
       $ legend.direction          : NULL
       $ legend.justification      : chr "center"
       $ legend.box                : NULL
       $ legend.box.just           : NULL
       $ legend.box.margin         : 'margin' num [1:4] 0cm 0cm 0cm 0cm
        ..- attr(*, "unit")= int 1
       $ legend.box.background     : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ legend.box.spacing        : 'simpleUnit' num 11points
        ..- attr(*, "unit")= int 8
       $ panel.background          :List of 5
        ..$ fill         : chr "white"
        ..$ colour       : logi NA
        ..$ linewidth    : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ panel.border              :List of 5
        ..$ fill         : logi NA
        ..$ colour       : chr "grey70"
        ..$ linewidth    : 'rel' num 1
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ panel.spacing             : 'simpleUnit' num 5.5points
        ..- attr(*, "unit")= int 8
       $ panel.spacing.x           : NULL
       $ panel.spacing.y           : NULL
       $ panel.grid                :List of 6
        ..$ colour       : chr "grey87"
        ..$ linewidth    : NULL
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ panel.grid.major          :List of 6
        ..$ colour       : NULL
        ..$ linewidth    : 'rel' num 0.5
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ panel.grid.minor          :List of 6
        ..$ colour       : NULL
        ..$ linewidth    : 'rel' num 0.25
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ panel.grid.major.x        : NULL
       $ panel.grid.major.y        : NULL
       $ panel.grid.minor.x        : NULL
       $ panel.grid.minor.y        : NULL
       $ panel.ontop               : logi FALSE
       $ plot.background           :List of 5
        ..$ fill         : NULL
        ..$ colour       : chr "white"
        ..$ linewidth    : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ plot.title                :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 1.2
        ..$ hjust        : num 0
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 5.5points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.title.position       : chr "panel"
       $ plot.subtitle             :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 5.5points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.caption              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : num 1
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 5.5points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.caption.position     : chr "panel"
       $ plot.tag                  :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 1.2
        ..$ hjust        : num 0.5
        ..$ vjust        : num 0.5
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.tag.position         : chr "topleft"
       $ plot.margin               : 'margin' num [1:4] 5.5points 5.5points 5.5points 5.5points
        ..- attr(*, "unit")= int 8
       $ strip.background          : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ strip.background.x        : NULL
       $ strip.background.y        : NULL
       $ strip.clip                : chr "inherit"
       $ strip.placement           : chr "inside"
       $ strip.text                :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : chr "white"
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 4.4points 4.4points 4.4points 4.4points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ strip.text.x              :List of 11
        ..$ family       : NULL
        ..$ face         : chr "bold"
        ..$ colour       : chr "black"
        ..$ size         : num 10
        ..$ hjust        : num 0
        ..$ vjust        : NULL
        ..$ angle        : num 90
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ strip.text.x.bottom       : NULL
       $ strip.text.x.top          : NULL
       $ strip.text.y              :List of 11
        ..$ family       : NULL
        ..$ face         : chr "bold"
        ..$ colour       : chr "black"
        ..$ size         : num 10
        ..$ hjust        : num 0
        ..$ vjust        : NULL
        ..$ angle        : num 0
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ strip.text.y.left         :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : num 90
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ strip.text.y.right        : NULL
       $ strip.switch.pad.grid     : 'simpleUnit' num 2.75points
        ..- attr(*, "unit")= int 8
       $ strip.switch.pad.wrap     : 'simpleUnit' num 2.75points
        ..- attr(*, "unit")= int 8
       - attr(*, "class")= chr [1:2] "theme" "gg"
       - attr(*, "complete")= logi TRUE
       - attr(*, "validate")= logi TRUE
      

