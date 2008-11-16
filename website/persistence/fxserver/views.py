from django.http import HttpResponse

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

