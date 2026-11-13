# Public detail page: full image and app language

Status: requested by the product owner on 2026-11-11.

## UX-PUBLIC-DETAIL-001

The public detail page of an event, profile, venue or classified shows a cropped preview of its image. When that image is real media, the preview is a control: activating it by pointer or keyboard opens a viewer that shows the whole image, uncropped, at the largest size the window allows. The viewer closes from its close control, from the image itself, from the backdrop and with Escape. The reference placeholder shown when there is no media is not a control. An image that fails to load in the viewer closes it and leaves the plain preview.

The page renders its own text and dates in the application language: Spanish unless the visitor chose another language in the application. The browser's language does not select the page language.

Component tests cover opening, closing, the inert placeholder, and the Spanish default under an English browser. Deployed verification on the public event page remains pending.
