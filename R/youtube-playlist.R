youtube_list <- function(video_df, playlist_id, example = FALSE) {
  if (example) {
    intro_p <- glue::glue("Cet ensemble de vidéos vous guide à travers les différentes étapes de l'exemple. Pour faciliter votre navigation, j'ai coupé les vidéos en segments plus digestes et les ai inclus dans une [chaîne YouTube](https://www.youtube.com/playlist?list={playlist_id}).")
  } else {
    intro_p <- glue::glue("Les vidéos pour cette section sont disponibles [sur cette chaîne YouTube](https://www.youtube.com/playlist?list={playlist_id}).")
  }

  videos_in_list <- dplyr::mutate(
    video_df,
    li = purrr::map2_chr(
      title, youtube_id, ~{
        glue::glue("- [{.x}](https://www.youtube.com/watch?v={.y}&list={playlist_id})")
      })
  )

  video_embed <- glue::glue('<div class="ratio ratio-16x9">
<iframe src="https://www.youtube.com/embed/playlist?list={playlist_id}" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>')

  final <- paste(intro_p,
                 paste(videos_in_list$li, collapse = "\n"),
                 "Vous pouvez aussi visionner les différents vidéos de la chaîne (en passant d'une vidéo à l'autre) ici:",
                 video_embed,
                 sep = "\n\n")

  cat(final)
}
