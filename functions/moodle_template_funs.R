



expandable_html_image = function(filename, click_message = "[Click to expand image]", thumb_width = NULL, lb_width = NULL, img_ref = NULL)
{
  
  if (FALSE)
  {
    filename = "github_desktop_sign_in.PNG"
    click_message = "[Click to expand image]"
    thumb_width = NULL
    thumb_width = "100px"
    img_ref = NULL
    lb_width = "80vw"
  }
  
  href = ifelse(
    is.null(img_ref),
    paste0("img", sample(9999999, 1)),
    img_ref)
  
  href_1 = 
  paste0(sample(letters, 12), collapse = "")
  
  img_path = list.files(path = here::here(), pattern = filename, recursive = TRUE, full.names = TRUE)[1]
  
  thumb_style =
    ifelse(
      is.null(thumb_width),
      "",
      sprintf('style="width: %s;"', thumb_width)
    )
  
  lb_style = 
    ifelse(
      is.null(lb_width),
      "",
      sprintf('style="width: %s;"', lb_width)
    )

  
  thumb_style_md =
    ifelse(
      is.null(thumb_width),
      "",
      sprintf('{width=%s}', thumb_width)
    )
    
  img_thumb_md = sprintf(fmt = '![](%s)%s', img_path, thumb_style_md)
  img_thumb_md = sprintf(fmt = '![](%s)%s', filename, thumb_style_md)
  
  
  img_lb_md = sprintf(fmt = '![](%s)', filename)
  
  
  fmt = paste(
    
    '<a href="#%6$s"></a>',
    '<a class="thumb" href="#%2$s">',
    '  <figure>',
    # '    <img %4$s src="%3$s"/>',
    '    %7$s',
    '    <figcaption>%1$s</figcaption>',
    '  </figure>',
    '</a>',
    '<div class="lb" %5$s id="%2$s">',
    # '  <img src="%3$s"/>',
    '  %8$s',
    '  <a class="lb-close" href="#%6$s"></a>',
    # '  <a class="lb-close" href="#"></a>',
    '</div>', 
    sep = "\n")
  
  
  # format text numberings:
  # 1  click message
  # 2  href code
  # 3  image file path
  # 4  thumbnail width
  # 5  lightbox width
  # 6  test href target for close
  # 7  markdown thumbnail image path
  # 8  markdown lightbos image path
  
  
  
  (out =
    sprintf(
    fmt = fmt,
    click_message, href, img_path, thumb_style, lb_style, href_1, img_thumb_md, img_lb_md))
  
  cat(out)
  invisible(out)
}




if (FALSE)
{
  
  expandable_html_image = function(filename, click_message = "[Click to expand image]", img_ref = NULL)
  {
    href = ifelse(
      is.null(img_ref),
      paste0("img", sample(9999999, 1)),
      img_ref)
    
    out = sprintf(fmt = 
                    '%1$s <br>
<a class="thumb" href="#%2$s">
![](%3$s)
</a> 
<div class="lb" id="%2$s">
![](%3$s)
<a class="lb-close" href="#"></a>
</div>', click_message, href, filename)
    cat(out)
  }
}



# expandable_html_image("abc")
