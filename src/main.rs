extern crate quick_xml;
extern crate xmlreorder;

use std::env;
use std::io::{self, BufRead, BufReader, Seek, SeekFrom, Write};
use std::fs::File;
use std::cmp;
use std::process;

use quick_xml::reader::Reader;
use xmlreorder::reorder;
use xmlreorder::ReorderingOptionsBuilder;

fn main() {

    let mut options = ReorderingOptionsBuilder::new();

    let mut input_or_example = None;
    let mut input_file = None;

    let mut it = env::args().skip(1);
    loop {
        match it.next() {
            Some(ref arg) if arg == "--filter-attributes" => {

                let xpath = it.next().expect("--filter-attributes XPATH ATTRIBUTE_NAME missing first argument (xpath)");
                let attr = it.next().expect("--filter-attributes XPATH ATTRIBUTE_NAME missing second argument (attribute name)");

                options.add_filtered_attribute(&xpath, &attr);
            },
            Some(ref help) if help == "--help" || help == "-h" => {
                println!("xmlreorder [args] [VERSIONED_FILE | EXAMPLE INPUT]");
                println!("Reorders given document based on an example, if possible. Outputs the ordered to standard out.");
                println!("Arguments:");
                println!();
                println!("  --filter-attributes XPATH ATTRIBUTE_NAME");
                println!("    Filter attribute name from both document when xpath is matched.");
                println!();
                println!("Operation modes:");
                println!("  - when only VERSIONED_FILE is given, load example from git, previous version");
                println!("  - when both EXAMPLE and FILE are given, input will be ordered by the example of EXAMPLE");
                println!("    - EXAMPLE can be hyphen (-) to mean STDIN");
                return;
            }
            Some(positioned) => {
                if input_or_example.is_none() {
                    input_or_example = Some(positioned);
                } else if input_file.is_none() {
                    input_file = Some(positioned);
                } else {
                    eprintln!("Too many positioned arguments: {}", positioned);
                    process::exit(1);
                }
            }
            None => break,
        }
    }

    match (input_or_example.as_ref(), input_file.as_ref()) {
        (Some(example), Some(_)) if example == "-" => unimplemented!(),
        (Some(example), Some(input)) => {
            let spans = reorder(
                Reader::from_file(example).expect("Failed to open example"),
                Reader::from_file(input).expect("Failed to open input"),
                options.build());

            let mut input = BufReader::new(File::open(input).expect("Failed to reopen input"));
            let stdout = io::stdout();
            let mut locked = stdout.lock();

            let mut pos = 0;

            for range in spans.into_iter().map(|x| x.to_range()) {
                if range.start != pos {
                    input.seek(SeekFrom::Start(range.start as u64)).expect("Input seeking failed");
                    pos = range.start;
                }

                while pos < range.end {
                    let amount = {
                        let buffer = input.fill_buf().expect("Reading input failed");

                        if buffer.len() == 0 {
                            panic!("eof before last span {} < {}", pos, range.end);
                        }

                        let needed = range.end - pos;
                        let amount = cmp::min(buffer.len(), needed);

                        locked.write_all(&buffer[0..amount]).expect("Writing to stdout failed");
                        amount
                    };

                    pos += amount;
                    input.consume(amount);
                }
            }
        },
        (Some(_input), None) => unimplemented!(),
        (None, Some(_)) => unreachable!(),
        (None, None) => {
            eprintln!("EXAMPLE and INPUT arguments are required");
            process::exit(1);
        }
    }


}
