Integrate witherable and lens.

Based on the ideas from https://chrispenner.ca/posts/witherable-optics
but I think this version has overall better ergonomics. This library
works around the issue that article describes as a "non-starter" with
use of an additional combinator to restore full compatibility with lens
combinators in the rare cases where that's required. By contrast, the
approach in the post fails when `f` isn't Alternative, which I find to
be very limiting.

TODO: Show all the examples from that post in this form
