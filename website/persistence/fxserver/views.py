from django.http import HttpResponse

def upload(request):
  try:
    src = request.REQUEST['src']
    request.session['_compilerSrc'] = src
    return HttpResponse('true')
  except KeyError:
    return HttpResponse('false')


def download(request):
  try:
    return HttpResponse(request.session['_compilerSrc']);
  except KeyError:
    return HttpResponse('"nothing uploaded"')

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

