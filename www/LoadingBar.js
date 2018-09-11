setInterval(function(){
                     if ($('html').attr('class')=='shiny-busy') {
                          setTimeoutConst = setTimeout(function(){
                            if ($('html').attr('class')=='shiny-busy') {
                             $('div.busy').show();
                           }  
                             }, delay = 300);
                           
                             
                      } else {
                          clearTimeout(setTimeoutConst);
                          $('div.busy').hide();
                      }
              } , 200);