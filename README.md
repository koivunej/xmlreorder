# xmlreorder

Utility that takes two versions of the same xml document, reorders the newer one following an example set by the older one.
Original use case for this is keeping xml documents in source control while having to use a tool which might, between version updates, reorder the file contents producing massive diffs.
Reordering any xml file by hand is not nice.

I hoped not to reinvent a wheel since this reordering by example and the whole XML business is more tricky than it first seems but could not find an existing tool.
Kudos for @tafia on yet another parser saving the day: [quick-xml](https://github.com/tafia/quick-xml)!

## Usage

Currently only supported way is to provide both the older version ("example") and the newer version ("input"):

```
xmlreorder example.xml input.xml
```

Reordered output will be written to standard output, so you'll need to manually store it and verify that nothing of importance was lost:

```
xmlreorder old.xml edited.xml > reordered.xml
diff -u old.xml reordered.xml    # make sure the diff is smaller
diff -u edited.xml reordered.xml # make sure nothing was lost or messed up
```

xmlreorder should be able to reorder arbitrary documents with arbitary depth but as it most of the data in starting elements (name, attributes) is kept in memory, you will eventually run out of memory.
I first experimented with a more online algoritm reading both input and example at the same time but that was even trickier.

## Issues

 * panic when root is mismatched
 * only utf8 has been tested
 * some elements might be left out (remember to diff the output to the input as well!)
 * very simple command line handling, only partial of features I'd like
 * xpath is really naive
 * algorithm probably misses out some points
 * indentation might be broken in some cases

If you find more, please try to build an example document showing the issue!

## Example run

`example.xml`:

```
<?xml version="1.0"?>
<example>
  <memorydump name="Foo">content1</memorydump>
  <memorydump name="Bar">content2</memorydump>
</example>
```

`input.xml`:

```
<?xml version"1.0"?>
<example>
  <memorydump name="Bar">content4</memorydump>
  <memorydump name="Foo">content3</memorydump>
</example>
```

Output:

```
<?xml version"1.0"?>
<example>
  <memorydump name="Foo">content3</memorydump>
  <memorydump name="Bar">content4</memorydump>
</example>
```

## Example with filtered attributes

Attributes can be an easy place to store metadata about the software which writes the files in random order.
Since elements are matched as `(path, order, name, attributes)` tuples inside `xmlreorder`, it can be helpful to filter out some attributes preventing matching.

`example2.xml`:

<?xml version="1.0"?>
<example version="0.1.2.3">
  <memorydump name="Foo">content1</memorydump>
  <memorydump name="Bar">content2</memorydump>
</example>
```

`input.xml`:

```
<?xml version"1.0"?>
<example version="1.2.3.4">
  <memorydump name="Bar">content4</memorydump>
  <memorydump name="Foo">content3</memorydump>
</example>
```

Output of `xmlreorder --filter-attributes '/example' 'version' example2.xml input2.xml`:

```
<?xml version"1.0"?>
<example version="1.2.3.4">
  <memorydump name="Foo">content3</memorydump>
  <memorydump name="Bar">content4</memorydump>
</example>
```
