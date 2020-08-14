require(here)



colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, x)
  } else x
}

bold_col <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textbf{\\textcolor{%s}{%s}}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, x)
  } else x
}

html_output_only = function(x)
{
  if (knitr::is_html_output()) {
    # cat(
    sprintf("<span>%s</span>", x)
    # )
  } 
}

expandable_html_image = function(
  filename, click_message = "[Click to expand image]", 
  thumb_width = 240, img_width = 60, cat_result = TRUE)
{
  # if(length(list.files(path = here::here(), pattern = filename)) == 0)
  # {
  #   tmp_name = list.files(path = here::here(), pattern = filename, recursive = TRUE, full.names = TRUE)
  #   stopifnot(length(tmp_name) > 0)
  #   filename = tmp_name[1]
  #   rm(tmp_name)
  # }
  # 
  if (FALSE)
  {
    # 
    # filename = "github_desktop_sign_in.PNG"
    # filename = "github_desktop_sign_in.PNG"
    filename = "mfn_github_profile_arrow.png"
    # 
    click_message = "[Click to expand image]"
    # 
    # img_width = 70
    img_width = 29
    # img_width = NULL
    thumb_width = 200
    # expandable_html_image(filename, click_message, img_width)
    expandable_html_image("mfn_github_profile_arrow.png")
    
    expandable_html_image("mfn_github_profile_arrow.png", img_width = 29)
  }
  
  href_image = paste0("img", sample(9999999, 1))
  href_close = paste0(sample(letters, 12), collapse = "")
  rmd_filename = sprintf(fmt = '![](%s)', filename)
  
  html_fmt = paste(
    '\n<a href="#%4$s"></a>',
    '<a class="thumb%6$s" href="#%3$s">',
    '  <figure>',
    '%2$s',
    '    <figcaption>%1$s</figcaption>',
    '  </figure>',
    '</a>',
    '<div class="lb%5$s" id="%3$s">',
    '%2$s',
    '  <a class="lb-close" href="#%4$s"></a>',
    '</div>\n', 
    sep = "\n")
  
  html_out = sprintf(
    html_fmt,
    click_message, 
    rmd_filename, 
    href_image, 
    href_close, 
    img_width,
    thumb_width)
  
  # format text numberings:
  # 1  click message
  # 2  image file name, formatted for markdown image inclusion
  # 3  href code for image
  # 4  href code for close
  # 5  lightbox image width
  # 6  thumb width
  
  
  lb_out = build_lightbox(img_width)
  thumb_out = build_thumb(thumb_width)

  
  
  
    
  out = paste("\n<style>\n", thumb_out, lb_out, "\n</style>\n", html_out)
  cat(out)
  if(cat_result) cat(thumb_out, lb_out, html_out)
  invisible(paste(thumb_out, lb_out, html_out, sep = "\n"))
}


build_thumb = function(thumb_width, cat_result = TRUE)
{
  
  # root_text = c(
  #   '\n:root {',
  #   '  --thumb-width: 280px;',
  #   '  --thumb-border: 1px solid green;',
  #   '  --thumb-box-shadow: 0px 0px 1.5vw rgba(0,0,0,1);',
  #   '  --thumb-margin: 10px 10px 10px 10px;',
  #   '  --lb-width: 100%1$s;',
  #   '  --lb-background: rgba(0,0,.3,0.4);',
  #   '  --lb-border: 1px solid red;',
  #   '  --lb-img-width: %2$s%1$s;',
  #   '  1px solid black; 1px solid black;',
  #   '  --lb-img-box-shadow: 0.3vw 0.3vw 0.3vw rgba(0,0,0,0.7);',
  #   '}\n')
  # 
  # root_block = sprintf(
  #   fmt = paste(
  #     '\n:root {',
  #     '  --thumb-width%2$s: %2$spx;',
  #     '  --thumb-border%2$s: 1px solid green;',
  #     '  --thumb-box-shadow%2$s: 0px 0px 1.5vw rgba(0,0,0,1);',
  #     '  --thumb-margin%2$s: 10px 10px 10px 10px;',
  #     # '  --lb-width: 100%2$s;',
  #     # '  --lb-background: rgba(0,0,.3,0.4);',
  #     # '  --lb-border: 1px solid red;',
  #     # '  --img-width: %1$s%2$s;',
  #     # '  --img-border:C 1px solid black;',
  #     # '  --img-box-shadow: 0.3vw 0.3vw 0.3vw rgba(0,0,0,0.7);',
  #     '}\n',
  #     sep = "\n"),
  #   "%", thumb_width)
  # 
  # style_block = sprintf(
  #   paste(
  #     '\n.thumb%2$s {',
  #     '    display: inline-block;',
  #     '    text-align: center;',
  #     '    margin: 10px 10px 10px 10px;',
  #     '}\n',
  #     '.thumb%2$s img {',
  #     '  width: var(--thumb-widtht%2$s);',
  #     '  border: var(--thumb-border%2$s);',
  #     '  box-shadow: var(--thumb-box-shadow%2$s);',
  #     '  margin: var(--thumb-margin%2$s);',
  #     '  display: block;',
  #     '}\n',
  #     sep = "\n"),
  #   "%", thumb_width)
  # 
  # 
  # cat(style_block)
  
  style_block = sprintf(
    paste(
      '\n.thumb%2$s {',
      '    display: inline-block;',
      '    text-align: center;',
      '    margin: 10px 10px 10px 10px;',
      '}\n',
      '.thumb%2$s img {',
      '  width: %2$spx;',
      '  border: 1px solid green;',
      '  box-shadow: 0px 0px 1.5vw rgba(0,0,0,1);',
      '  margin: 10px 10px 10px 10px;',
      '  display: block;',
      '}\n',
      sep = "\n"),
    "%", thumb_width)
  
  out = style_block
  
  # out = paste0("\n<style>\n", root_block, style_block, "\n</style>\n")
  # out = list(root = root_block, style = style_block)
  # cat(out)
  return(out)
}

build_lightbox = function(img_width)
{
  
  
  
  
  # root_text = c(
  #   '\n:root {',
  #   '  --thumb-width: 280px;',
  #   '  --thumb-border: 1px solid green;',
  #   '  --thumb-box-shadow: 0px 0px 1.5vw rgba(0,0,0,1);',
  #   '  --thumb-margin: 10px 10px 10px 10px;',
  #   '  --lb-width: 100%1$s;',
  #   '  --lb-background: rgba(0,0,.3,0.4);',
  #   '  --lb-border: 1px solid red;',
  #   '  --lb-img-width: %2$s%1$s;',
  #   '  1px solid black; 1px solid black;',
  #   '  --lb-img-box-shadow: 0.3vw 0.3vw 0.3vw rgba(0,0,0,0.7);',
  #   '}\n')
  # 
  # 
  # 
  # 
  # root_block = sprintf(
  #   fmt = paste(
  #     '\n:root {',
  #     # '  --thumb-width: 280px;',
  #     # '  --thumb-border: 1px solid green;',
  #     # '  --thumb-box-shadow: 0px 0px 1.5vw rgba(0,0,0,1);',
  #     # '  --thumb-margin: 10px 10px 10px 10px;',
  #     '  --lb-width: 100%1$s;',
  #     '  --lb-background: rgba(0,0,.3,0.4);',
  #     # '  --lb-border: 1px solid red;',
  #     '  --lb-img-width: %2$s%1$s;',
  #     '  1px solid black; 1px solid black;',
  #     '  --lb-img-box-shadow: 0.3vw 0.3vw 0.3vw rgba(0,0,0,0.7);',
  #     '}\n',
  #     sep = "\n"),
  #   "%", img_width)
  
  
  left = (100 - img_width) * 0.5
  
  style_block  =
    sprintf(
      fmt = paste(
        '\n.lb%2$s {',
        'border: 1px solid red;',
        'position: fixed; bottom: 0; left 0; width: 100%1$s;',
        'height: auto; overflow: auto; text-align: center;',
        # 'left: %2$s%1$s * 0.5);',
        # 'left: calc(50%2$s - 100%1$s * 0.5);',
        'background: rgba(0,0,.3,0.4); opacity: 0;',
        '}\n',
        
        '.lb%2$s img {',
        'position: absolute; object-fit: contain;',
        'right: 0; bottom: 0; top: 0;',
        'left: %3$s%1$s;',
        # 'left: calc(0.5 * (100%2$s - var(--lb-img-width%1$s)));',
        'width: %2$s%1$s; height: auto;',
        'box-shadow: 0.3vw 0.3vw 0.3vw rgba(0,0,0,0.7);',
        'border: 1px solid black;',
        'text-align: center;',
        '}\n',
        
        '.lb%2$s:target { opacity: 1; top: 0; bottom: 0; }',
        '.lb%2$s:target img { max-height: 100%1$s; max-width: 100%1$s; }',
        '.lb%2$s:target .lightbox-close { top: 0; }\n',
        
        '.lb-close {',
        '  position: absolute; display: block;',
        '  width:100%1$s; height:100%1$s;  top: 0; right: 0;',
        '  box-sizing: border-box; background: transparent;',
        '  color: black; text-decoration: none;',
        '}\n',
        sep = "\n"),
      "%", img_width, left)
  
  # cat(style_block)
  out = style_block
  # out = paste0("\n<style>\n", root_block, style_block, "\n</style>\n")
  # out = list(root = root_block, style = style_block)
  # cat(out)
  
  return(out)
  
  # fmt_lb = paste(
  #   '<style>',
  #   ':root {',
  #   '  --img-width%1$s: %1$s%2$s;',
  #   '}',
  #   '.lb%1$s {',
  #   'border: var(--lb-border);',
  #   'position: fixed;',
  #   'height: auto; overflow: auto; text-align: center;',
  #   'bottom: 0;',
  #   'left: calc(50%2$s - var(--lb-width) * 0.5);',
  #   'width: var(--lb-width);',
  #   'background: var(--lb-background); opacity: 0;',
  #   '}',
  #   '.lb%1$s img {',
  #   'position: absolute; object-fit: contain;',
  #   'right: 0;',
  #   'bottom: 0;',
  #   'top: 0;',
  #   'left: calc(0.5 * (100%2$s - var(--img-width%1$s)));',
  #   'width: var(--img-width%1$s); height: auto;',
  #   'box-shadow: var(--img-box-shadow);',
  #   'border: var(--img-border);',
  #   'text-align: center;',
  #   '}',
  #   '.lb%1$s:target { opacity: 1; top: 0; bottom: 0; }',
  #   '.lb%1$s:target img { max-height: 100%2$s; max-width: 100%2$s; }',
  #   '.lb%1$s:target .lightbox-close { top: 0; }',
  #   '</style>', "",
  #   sep = "\n")
  # 
  # return(sprintf(fmt = fmt_lb, img_width, "%"))
}


expandable_html_image_ = function(filename, click_message = "[Click to expand image]", img_width = NULL, cat_result = TRUE)
{
  # if(length(list.files(path = here::here(), pattern = filename)) == 0)
  # {
  #   tmp_name = list.files(path = here::here(), pattern = filename, recursive = TRUE, full.names = TRUE)
  #   stopifnot(length(tmp_name) > 0)
  #   filename = tmp_name[1]
  #   rm(tmp_name)
  # }
  # 
  if (FALSE)
  {
    # 
    # filename = "github_desktop_sign_in.PNG"
    # filename = "github_desktop_sign_in.PNG"
    # filename = "mfn_github_profile_arrow.png"
    # 
    # click_message = "[Click to expand image]"
    # 
    # img_width = 70
    # img_width = 29
    # img_width = NULL
    # expandable_html_image(filename, click_message, img_width)
    expandable_html_image("mfn_github_profile_arrow.png")
    
    expandable_html_image("mfn_github_profile_arrow.png", img_width = 29)
  }
  
  href_image = paste0("img", sample(9999999, 1))
  href_close = paste0(sample(letters, 12), collapse = "")
  rmd_filename = sprintf(fmt = '![](%s)', filename)
  
  lb_class_name = ifelse(
    is.null(img_width),
    "lb",
    paste0("lb", img_width)
  )
  
  fmt = paste(
    '<a href="#%4$s"></a>',
    '<a class="thumb" href="#%3$s">',
    '  <figure>',
    '    %2$s',
    '    <figcaption>%1$s</figcaption>',
    '  </figure>',
    '</a>',
    '<div class="%5$s" id="%3$s">',
    '  %2$s',
    '  <a class="lb-close" href="#%4$s"></a>',
    '</div>', 
    sep = "\n")
  
  # format text numberings:
  # 1  click message
  # 2  image file name, formatted for markdown image inclusion
  # 3  href code for image
  # 4  href code for close
  # 5  custom lightbox class name
  
  out =
    sprintf(
      fmt = fmt,
      click_message, rmd_filename, href_image, href_close, lb_class_name )
  
  if (!is.null(img_width))
    out = paste0(build_lightbox(img_width), out)
  
  if(cat_result) cat(out)
  invisible(out)
}

