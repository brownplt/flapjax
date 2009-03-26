from django.http import HttpResponse

import settings
import subprocess

if settings.host == "peabody":
  fxc = "fxc"
elif settings.host == "wanderlust.local":
  fxc = "fxc"
else:
  fxc = "/home/flapjax/compiler/bin/fxc"

flapjaxCmd = [fxc,"--flapjax","/fx/flapjax.js","--stdin","--stdout",
              "--web-mode"]

# The "Try Flapjax" page uses setobj to store the page at setobj/tryFlapjax.
# The compiler does the same.  The result is a bunch of warnings.
def compile(request):
  p = subprocess.Popen(flapjaxCmd,stdout=subprocess.PIPE,
                       stderr=subprocess.PIPE,stdin=subprocess.PIPE,
                       close_fds=True)
  p.stdin.write(request.raw_post_data)
  p.stdin.close()
  result = p.wait()
  if result == 0:
    request.session["tryFlapjax"] = p.stdout.read()
  response = p.stderr.read()
  p.stderr.close()
  p.stdout.close()
  return HttpResponse(response)
  
def getobj(request,obj_id):
  try:
    return HttpResponse(request.session[obj_id])
  except KeyError:
    request.session[obj_id] = request.raw_post_data
    return HttpResponse(request.raw_post_data)

def setobj(request,obj_id):
  try:
    request.session[obj_id] = request.raw_post_data
    return HttpResponse('true')
  except KeyError, val:
    return HttpResponse(val.__str__())

