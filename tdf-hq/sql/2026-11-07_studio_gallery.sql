-- Register the studio gallery in the existing versioned CMS. The first revision
-- contains owner-selected local assets; reruns never overwrite editorial changes.
-- Apply after catalog foundation and the CMS consumer expansion migrations.
BEGIN;
SET LOCAL lock_timeout = '10s';
INSERT INTO content_type
 (id,code,entity_kind,name_es,name_en,schema_json,schema_version,public_route_pattern,admin_route_pattern,public_read,active,workflow_state_id,version)
VALUES ('ed195635-f447-44cb-a547-f79ef1dcbda2','studio-photo-gallery','authored_page','Galería del estudio','Studio gallery',
 $gallery_schema${"type": "object", "required": ["schemaVersion", "photos"], "properties": {"schemaVersion": {"const": 1}, "photos": {"type": "array", "maxItems": 200, "items": {"type": "object", "required": ["id", "title", "alt", "imageUrl", "visible"]}}}, "example": {"schemaVersion": 1, "photos": []}}$gallery_schema$,1,'/tdf#estudio','/configuracion/cms?slug=studio-gallery',true,true,'00000000-0000-4000-8000-000000000205',1)
ON CONFLICT DO NOTHING;
INSERT INTO authored_content
 (id,content_type_id,code,name_es,name_en,current_slug,public_route,sort_order,active,workflow_state_id,published_revision,version)
VALUES ('2368bb35-3ff7-45b3-96f3-425e11812544','ed195635-f447-44cb-a547-f79ef1dcbda2','studio-gallery','Galería del estudio','Studio gallery','studio-gallery','/tdf#estudio',30,true,'00000000-0000-4000-8000-000000000205',1,1)
ON CONFLICT DO NOTHING;
INSERT INTO catalog_slug_alias
 (id,catalog_id,entity_kind,entity_id,scope,slug,current,redirect_status)
VALUES ('fcdab8cb-5f45-400f-8f0b-00e60f620c16','10000000-0000-4000-8000-000000000031','authored_content','2368bb35-3ff7-45b3-96f3-425e11812544','cms-public','studio-gallery',true,308)
ON CONFLICT DO NOTHING;
INSERT INTO cms_content
 (authored_content_id,slug,locale,version,status,title,payload,created_at,updated_at,published_at)
SELECT '2368bb35-3ff7-45b3-96f3-425e11812544','studio-gallery','es',1,'published','Galería del estudio',
 $studio_gallery_seed${
  "schemaVersion": 1,
  "photos": [
    {
      "id": "maps-preamps",
      "title": "Procesadores y preamplificadores",
      "alt": "Rack iluminado con preamplificadores y compresores API, Universal Audio y Avalon.",
      "imageUrl": "/assets/tdf-studio/maps-preamps-1280.webp",
      "thumbnailUrl": "/assets/tdf-studio/maps-preamps-640.webp",
      "credit": "Daniel Escudero · Google Maps",
      "capturedAt": "2024-06",
      "sourceUrl": "https://www.google.com/maps/place/TDF+Records/@-0.1578688,-78.4918266,3a,75y,90t/data=!3m4!1e2!3m2!1sCIHM0ogKEICAgICz_9WLGg!2e10",
      "visible": true
    },
    {
      "id": "maps-control-room",
      "title": "Sala de control · Google Maps",
      "alt": "Consola, pantallas y racks de procesadores en la sala de control de TDF.",
      "imageUrl": "/assets/tdf-studio/maps-control-room-1280.webp",
      "thumbnailUrl": "/assets/tdf-studio/maps-control-room-640.webp",
      "credit": "Daniel Escudero · Google Maps",
      "capturedAt": "2024-06",
      "sourceUrl": "https://www.google.com/maps/place/TDF+Records/@-0.1578688,-78.4918266,3a,75y,90t/data=!3m4!1e2!3m2!1sCIHM0ogKEICAgICz_7WYugE!2e10",
      "visible": true
    },
    {
      "id": "maps-dj-room",
      "title": "Cabina DJ y sala",
      "alt": "Cabina con dos reproductores Pioneer y mezclador frente a la batería y las guitarras del estudio.",
      "imageUrl": "/assets/tdf-studio/maps-dj-room-1280.webp",
      "thumbnailUrl": "/assets/tdf-studio/maps-dj-room-640.webp",
      "credit": "Daniel Escudero · Google Maps",
      "capturedAt": "2024-06",
      "sourceUrl": "https://www.google.com/maps/place/TDF+Records/@-0.1578688,-78.4918266,3a,75y,90t/data=!3m4!1e2!3m2!1sCIHM0ogKEICAgICz_-XHZg!2e10",
      "visible": true
    },
    {
      "id": "maps-analog-rack",
      "title": "Equipos analógicos",
      "alt": "Procesadores Rupert Neve y equipos EMI Curve Bender y Zener Limiter en el rack del estudio.",
      "imageUrl": "/assets/tdf-studio/maps-analog-rack-1280.webp",
      "thumbnailUrl": "/assets/tdf-studio/maps-analog-rack-640.webp",
      "credit": "Daniel Escudero · Google Maps",
      "capturedAt": "2024-06",
      "sourceUrl": "https://www.google.com/maps/place/TDF+Records/@-0.1578688,-78.4918266,3a,75y,90t/data=!3m4!1e2!3m2!1sCIHM0ogKEICAgICz_9WPbw!2e10",
      "visible": true
    },
    {
      "id": "rojo-live-room",
      "title": "Roj(o) en vivo en TDF",
      "alt": "La banda Roj(o) tocando en la sala de grabación, iluminada en rojo, con batería, bajo y guitarra.",
      "imageUrl": "/assets/tdf-studio/rojo-live-room-1280.webp",
      "thumbnailUrl": "/assets/tdf-studio/rojo-live-room-640.webp",
      "visible": true,
      "capturedAt": "2025-02-24",
      "credit": "Roj(o) · Fotograma de la sesión · Realización audiovisual: Qlandestina",
      "sourceUrl": "https://www.instagram.com/p/DfLV5lMCnfX/"
    },
    {
      "id": "rojo-vocal-session",
      "title": "Voz y guitarra en sesión",
      "alt": "Vocalista de Roj(o) con audífonos cantando frente al micrófono y tocando la guitarra.",
      "imageUrl": "/assets/tdf-studio/rojo-vocal-session-1280.webp",
      "thumbnailUrl": "/assets/tdf-studio/rojo-vocal-session-640.webp",
      "visible": true,
      "capturedAt": "2025-02-24",
      "credit": "Roj(o) · Fotograma de la sesión · Realización audiovisual: Qlandestina",
      "sourceUrl": "https://www.instagram.com/p/DfLV5lMCnfX/"
    },
    {
      "id": "rojo-drum-recording",
      "title": "Batería en vivo",
      "alt": "Baterista de Roj(o) tocando en la sala con micrófonos alrededor de los platillos y tambores.",
      "imageUrl": "/assets/tdf-studio/rojo-drum-recording-1280.webp",
      "thumbnailUrl": "/assets/tdf-studio/rojo-drum-recording-640.webp",
      "visible": true,
      "capturedAt": "2025-02-24",
      "credit": "Roj(o) · Fotograma de la sesión · Realización audiovisual: Qlandestina",
      "sourceUrl": "https://www.instagram.com/p/DfLV5lMCnfX/"
    },
    {
      "id": "control-room",
      "title": "Sala de control",
      "alt": "Consola de mezcla frente a dos pantallas y monitores de estudio.",
      "imageUrl": "/assets/tdf-studio/control-room-1280.webp",
      "thumbnailUrl": "/assets/tdf-studio/control-room-640.webp",
      "visible": true
    },
    {
      "id": "recording-room",
      "title": "Sala de grabación",
      "alt": "Sala con batería, guitarras, amplificadores y micrófonos.",
      "imageUrl": "/assets/tdf-studio/recording-room-1280.webp",
      "thumbnailUrl": "/assets/tdf-studio/recording-room-640.webp",
      "visible": true
    },
    {
      "id": "instruments",
      "title": "Instrumentos y sonido",
      "alt": "Sintetizadores, micrófono y equipos junto a la consola de mezcla.",
      "imageUrl": "/assets/tdf-studio/instruments-1280.webp",
      "thumbnailUrl": "/assets/tdf-studio/instruments-640.webp",
      "visible": true
    },
    {
      "id": "mixing-detail",
      "title": "Trabajo en consola",
      "alt": "Mano ajustando los faders de una consola de mezcla.",
      "imageUrl": "/assets/tdf-studio/mixing-detail-1280.webp",
      "thumbnailUrl": "/assets/tdf-studio/mixing-detail-640.webp",
      "visible": true
    },
    {
      "id": "drum-session",
      "title": "Una sesión de batería",
      "alt": "Músico tocando una batería rodeada de micrófonos en el estudio.",
      "imageUrl": "/assets/tdf-studio/drum-session-1280.webp",
      "thumbnailUrl": "/assets/tdf-studio/drum-session-640.webp",
      "visible": true
    },
    {
      "id": "studio-community",
      "title": "La música nos reúne",
      "alt": "Grupo de músicos reunidos junto a la consola en el estudio.",
      "imageUrl": "/assets/tdf-studio/studio-community-1280.webp",
      "thumbnailUrl": "/assets/tdf-studio/studio-community-640.webp",
      "visible": true
    }
  ]
}$studio_gallery_seed$,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP
WHERE NOT EXISTS (SELECT 1 FROM cms_content WHERE slug='studio-gallery' AND locale='es');
COMMIT;
