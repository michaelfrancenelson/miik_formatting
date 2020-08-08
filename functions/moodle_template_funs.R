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



# expandable_html_image("abc")
