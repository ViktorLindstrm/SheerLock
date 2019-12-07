-module(layout).
-export([rp_desc/5,content/2,content/1,header/1,client/3,headers/0]).

content(Content) -> 
    ["<html>",headers(),
    "<body>",
     Content,
     "</body>",
     "</html>"
    ].

content(Left,Right) -> 
    ["<html>",headers(),
     "<body>",
     "<div class=\"container-fluid\">
      <div class=\"row\">
        <div class=\"col-4\">",
         Left 
     ,"</div>
        <div class=\"col-8\">",
         Right
     ,"</div>
      </div>
    </div>",
     "</body>",
     "</html>"
    ].
header(Focus) -> 
    ["<header>",
         "<nav class=\"navbar navbar-expand-md navbar-dark fixed-top bg-dark\">",
         "<a class=\"navbar-brand\" href=\"/\">SheerLock IDP - ",Focus,"</a>",
         "<button class=\"navbar-toggler\" type=\"button\" data-toggle=\"collapse\" data-target=\"#navbarCollapse\" aria-controls=\"navbarCollapse\" aria-expanded=\"false\" aria-label=\"Toggle navigation\">",
             "<span class=\"navbar-toggler-icon\"></span>",
         "</button>",
         "<div class=\"collapse navbar-collapse\" id=\"navbarCollapse\">",
             "<ul class=\"navbar-nav mr-auto\">",
             "</ul>",
         "</div>",
         "</nav>",
     "</header>"].

headers() ->
    ["<head>",
    "<meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\">",

    "<link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css\" integrity=\"sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T\" crossorigin=\"anonymous\">",
    "<script src=\"https://code.jquery.com/jquery-3.3.1.slim.min.js\" integrity=\"sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo\" crossorigin=\"anonymous\"></script>",
    "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js\" integrity=\"sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1\" crossorigin=\"anonymous\"></script>",
    "<script src=\"https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js\" integrity=\"sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM\" crossorigin=\"anonymous\"></script>",

    "<style>
      .bd-placeholder-img {
        font-size: 1.125rem;
        text-anchor: middle;
        -webkit-user-select: none;
        -moz-user-select: none;
        -ms-user-select: none;
        user-select: none;
      }
    /* Move down content because we have a fixed navbar that is 3.5rem tall */
    body {
      padding-top: 7rem;
      padding: 7rem;
    }
      @media (min-width: 768px) {
        .bd-placeholder-img-lg {
          font-size: 3.5rem;
        }
      }
    </style>",
    "</head>"].


client(Name,ClientID,Desc) ->
    ["<div class=\"card\" style=\"width: 18rem;\">
      <div class=\"card-body\">
        <h5 class=\"card-title\">",Name,"</h5>
        <h6 class=\"card-subtitle mb-2 text-muted\">Client id: ",ClientID,"</h6>
        <p class=\"card-text\">",Desc,"</p>
        <a href=\"/admin/",ClientID,"\" class=\"card-link\">Settings</a>
      </div>
    </div>"].


rp_desc(Name,Desc,RefreshTime,ClientId,CodeSize) ->
    [
    "<dl class=\"row\">
      <dt class=\"col-3\">Name</dt>
      <dd class=\"col-9\">",Name,"</dd>

      <dt class=\"col-3\">Client id</dt>
      <dd class=\"col-9\">",ClientId,"</dd>

      <dt class=\"col-3\">Description</dt>
      <dd class=\"col-9\">",Desc,"</dd>

      <dt class=\"col-3\">Expiration time for Refresh tokens</dt>
      <dd class=\"col-9\">",RefreshTime,"</dd>

      <dt class=\"col-3\">Length of Code in Authorization Code Flow</dt>
      <dd class=\"col-9\">",CodeSize,"</dd>

    </dl>"
    ].

