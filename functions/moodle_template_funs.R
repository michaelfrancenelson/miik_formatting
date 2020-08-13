

expandable_html_image = function(filename, click_message = "[Click to expand image]", img_width = NULL)
{
  
  if (FALSE)
  {
    filename = "github_desktop_sign_in.PNG"
    click_message = "[Click to expand image]"
    img_width = 70
    expandable_html_image(filename, click_message, img_width)
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
  
  (out =
      sprintf(
        fmt = fmt,
        click_message, rmd_filename, href_image, href_close, lb_class_name ))
  
  if (!is.null(img_width))
    out = paste0(build_lightbox(img_width), out)
  
  cat(out)
  invisible(out)
}

build_lightbox = function(img_width)
{
  fmt_lb = paste(
    '<style>',
    ':root {',
    '  --img-width%1$s: %1$s%2$s;',
    '}',
    '.lb%1$s {',
    'border: var(--lb-border);',
    'position: fixed;',
    'height: auto; overflow: auto; text-align: center;',
    'bottom: 0;',
    'left: calc(50%2$s - var(--lb-width) * 0.5);',
    'width: var(--lb-width);',
    'background: var(--lb-background); opacity: 0;',
    '}',
    '.lb%1$s img {',
    'position: absolute; object-fit: contain;',
    'right: 0;',
    'bottom: 0;',
    'top: 0;',
    'left: calc(0.5 * (100%2$s - var(--img-width%1$s)));',
    'width: var(--img-width%1$s); height: auto;',
    'box-shadow: var(--img-box-shadow);',
    'border: var(--img-border);',
    'text-align: center;',
    '}',
    '.lb%1$s:target { opacity: 1; top: 0; bottom: 0; }',
    '.lb%1$s:target img { max-height: 100%2$s; max-width: 100%2$s; }',
    '.lb%1$s:target .lightbox-close { top: 0; }',
    '</style>',
    sep = "\n")
  
  return(sprintf(fmt = fmt_lb, img_width, "%"))
}
