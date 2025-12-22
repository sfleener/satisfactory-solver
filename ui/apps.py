from django.apps import AppConfig

class UiConfig(AppConfig):
    default_auto_field = 'django.db.models.BigAutoField'
    name = 'ui'

    def ready(self):
        # Seed a default record if none exists
        try:
            from .models import DefaultSettings
            from django.db.utils import OperationalError, ProgrammingError
            import json, os

            if DefaultSettings.objects.exists():
                return

            base_dir = os.path.dirname(os.path.abspath(__file__))
            default_path = os.path.join(base_dir, 'data', 'default.json')
            if os.path.exists(default_path):
                with open(default_path, 'r', encoding='utf-8') as f:
                    raw = json.load(f)
                DefaultSettings.objects.create(
                    phase=raw.get("phase", 5),
                    resource_limits=raw.get("resource_limits", {}),
                    weights=raw.get("weights", {}),
                    recipes_off=raw.get("recipes_off", []),
                    inputs=raw.get("inputs", {}),
                    outputs=raw.get("outputs", {}),
                    max_item=raw.get("max_item") or None,
                    checkbox_Nuclear_Waste=raw.get("checkbox_Nuclear Waste", False),
                    integer_recipes=raw.get("integer_recipes", True),
                )
        except (OperationalError, ProgrammingError):
            # migrations not yet run
            pass
