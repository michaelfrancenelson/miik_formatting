require(here)


find_img = function(filename)
{
  matching_files = list.files(path = here::here(), pattern = filename, recursive = TRUE, full.names = TRUE)
  stopifnot(length(matching_files) > 0)
  return(matching_files[1])
}

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
    sprintf("<span>%s</span>", x)
  } 
}

expandable_html_image = function(
  filename, click_message = "[Click to expand image]", 
  thumb_width = 240, img_width = 60, cat_result = TRUE, moodle_quiz = FALSE)
{
  if (FALSE)
  {
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
  
  if (!moodle_quiz)
  {
    matching_files = list.files(path = here::here(), pattern = filename, recursive = TRUE, full.names = TRUE)
    stopifnot(length(matching_files) > 0)
    filename = matching_files[1]
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
    '  <a class="lb-close%5$s" href="#%4$s"></a>',
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
  
  if(cat_result) 
    cat(out)
  invisible(out)
}


build_thumb = function(thumb_width, cat_result = TRUE)
{
  thumb = sprintf(
    paste(
      '\n.thumb%1$s img {',
      'width: %1$spx;',
      'border: 1px solid green;',
      'box-shadow: 0px 0px 1.5vw rgba(0,0,0,1);',
      '}\n', sep = "\n"),
    thumb_width)
  
  return(thumb)
}

build_lightbox = function(img_width)
{
  lb = sprintf(
    fmt = paste(
      '\n.lb%2$s {',
      'position: fixed;',
      'top: -100%1$s;',
      'width: 100%1$s;',
      'background: rgba(0,0,0,0.4);',
      '}\n', sep = "\n"),
    "%", img_width)
  
  lb_img = sprintf(
    fmt = paste(
      '\n.lb%2$s img {',
      'margin: auto;',
      'position: absolute; object-fit: contain;',
      'right: 0; left: 0; bottom: 0; top: 0;',
      'width: %2$s%1$s;',
      '}\n', sep = "\n"),
    "%", img_width)
  
  lb_close = sprintf(
    fmt = paste(
      '\n.lb-close%2$s {',
      'display: block; position: absolute;',
      'width: 100%1$s; height: 100%1$s;',
      '}\n', sep = "\n"),
    "%", img_width)
  
  lb_target = sprintf(
    fmt = paste(
      '\n.lb%2$s:target {',
      'opacity: 1; top: 0; bottom: 0;',
      '}\n', sep = "\n"),
    "%", img_width)
  
  out = paste("\n", lb, lb_img, lb_close, lb_target)
  return(out)
}


build_popup = function(filename, thumb_width = 250, cat_output = TRUE)
{
  fmt_popup = 
    '<a target="_blank" href="%1$s"><img src="install_rstudio_question.PNG" style="width:%2$spx"></a>'
  out_popup = sprintf(fmt_popup, filename, thumb_width)
  if(cat_output) cat(out_popup)
  invisible(out_popup)
}