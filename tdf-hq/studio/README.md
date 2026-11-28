# Studio photo submissions

Open **Configuración → CMS → Galería del estudio** and, when starting from the published version, select **Usar versión en vivo**. Select **Revisar contribuciones y gestionar enlace**. Select **Crear enlace** and **Copiar enlace**. Contributors can open that address on a phone or computer without signing in. Creating a replacement invalidates the old upload link immediately. Disabling submissions leaves batch receipts and existing photos accessible under their original permissions.

Reviewers can preview a photo, reject it, or incorporate it into the gallery draft. Select individual photos or a whole batch. Review the descriptions, credits and order in the editor, save the draft, and have another authorized administrator or webmaster publish it. The public studio gallery is at `/tdf#estudio`.

The API stores the intake database on the private, persistent `/app/uploads/studio-submissions` mount. Include this mount in the existing coordinated backup and restore. The private database holds the byte-for-byte originals, review receipts, and 640- and 1600-pixel JPEG versions. Published URLs check the visible Spanish CMS gallery before serving images. Link and receipt secrets travel in URL fragments on the web and in request headers or bodies at the API.

The intake limit is 30 positions per batch and 25 MiB per file. The shared limits are 5 GiB of photo data, 100 new batches per hour and 240 decode attempts per hour. Daily retention clears pending or rejected file data after 30 days, removes expired or rejected receipts after 90 days, and clears empty batches after one day. Approved originals are retained. Each image is validated and decoded before storage. HEIC/HEIF rendering uses the primary image.

For local API development, run from `tdf-hq` with Python 3, Pillow and `heif-thumbnailer` on the path. Both backend runtime images include these dependencies. Run `python3 studio/test_submissions.py` for storage and review checks.

Photos are decoded inside the API container, which has 512 MiB. A file may be up to 25 MiB. JPEG is decoded at reduced scale, so camera photos up to 50 megapixels are accepted. HEIC is converted by `heif-thumbnailer` at the largest stored size. PNG is refused above 13 megapixels and WebP above 8, because neither can be decoded at reduced scale. The test suite checks that a 50-megapixel JPEG is processed in under 256 MiB.

## Release order

Release the reviewed backend and gallery migration first. Before promoting the web build, verify that the production API returns a published Spanish gallery with the expected existing photos from `GET /cms/content?slug=studio-gallery&locale=es`. A missing or empty record on first rollout leaves the current web deployment in service. Verify that signed-out queue access returns 401, then promote the web build. Staff can now create the unlisted invitation. Confirm a pending submission stays private and review a real photo before publishing it through the normal CMS approval flow.
