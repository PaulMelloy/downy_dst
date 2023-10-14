ccs_style <- function(n) {
   if(n == 1){
      return(tags$head(tags$style(
         HTML(
            "
            code {
                display:block;
                padding:20px;
                margin:0 0 10px;
                margin-top:10px;
                font-size:33px;
                line-height:15px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#faa0a0;
                border:2px solid rgba(0.3,0,0.01,0.7);
                border-radius:4px;
                font-family:arial;
            }"
         )
      )))
   }
   if(n == 2){
      return(
      tags$head(tags$style(
         HTML(
            "
            code {
                display:block;
                padding:20px;
                margin:0 0 10px;
                margin-top:10px;
                font-size:33px;
                line-height:15px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#e1ffc4;
                border:2px solid rgba(0.3, 0.5, 0.122,0.7);
                border-radius:4px;
                font-family:arial;
            }"
         )
      )))
   }
   if(n == 3){
      return(tags$head(tags$style(
         HTML(
            "
            code {
                display:block;
                padding:20px;
                margin:0 0 10px;
                margin-top:10px;
                font-size:33px;
                line-height:15px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#d9eaff;
                border:2px solid rgba(0.01,0,0.3,0.7);
                border-radius:4px;
                font-family:arial;
            }"
         )
      )))
   }
}
