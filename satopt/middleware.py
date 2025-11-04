# satopt/middleware.py
from django.shortcuts import redirect

CANONICAL_HOST = "satisfactory-solver.com"

class CanonicalDomainMiddleware:
    def __init__(self, get_response):
        self.get_response = get_response

    def __call__(self, request):
        host = request.get_host().split(":")[0].lower()

        if host == CANONICAL_HOST:
            return self.get_response(request)

        # Redirect www. → apex
        if host == f"www.{CANONICAL_HOST}":
            return redirect(
                f"https://{CANONICAL_HOST}{request.get_full_path()}",
                permanent=True
            )

        return self.get_response(request)
