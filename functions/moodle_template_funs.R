

expandable_html_image = function(filename, click_message = "[Click to expand image]", thumb_width = NULL, img_ref = NULL)
{
  href = ifelse(
    is.null(img_ref),
    paste0("img", sample(9999999, 1)),
    img_ref)
  
  thumb_width = ifelse(
    is.null(thumb_width),
    "",
    paste0("{width=", thumb_width, "}")
  )
  
  
  fmt = paste0(
    '%1$s <br>',
    '<a class="thumb" href="#%2$s">',
    '![](%3$s)%4$s',
    '</a>',
    '<div class="lb" id="%2$s">',
    '![](%3$s)',
    '<a class="lb-close" href="#"></a>',
    '</div>', sep = "\n")
  
  out = sprintf(
    fmt = fmt,
    click_message, href, filename, thumb_width)
  
  
  
  
  out_ = sprintf(fmt = 
                   '%1$s <br>
                <a class="thumb" href="#%2$s">
                ![](%3$s)
                </a> 
                <div class="lb" id="%2$s">
                ![](%3$s)
                <a class="lb-close" href="#"></a>
                </div>', click_message, href, filename)
  
  # cat(out)
  return(out)
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
