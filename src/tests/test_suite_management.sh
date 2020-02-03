setup_suite () {
	# echo "BEGIN SETUP"
	# prep the disposable test directory
	TEST_COMMENTS_DIR="$HOME/pc_test_comments_dir"
	if [ -e $TEST_COMMENTS_DIR ]; then
		# echo "deleting $TEST_COMMENTS_DIR from last run"
		rm -rf $TEST_COMMENTS_DIR
	fi
	# mkdir -p $TEST_COMMENTS_DIR

	# if the server is running, kill it
	PID=$(pgrep private_comments)
	if [ "$PID" != "" ]; then
		# echo "Shutting down current private_comments server"
		pkill private_comments
	fi

	# echo "starting server"
	# start it and have it use the disposable directory
	PRIVATE_COMMENTS_DIR=$TEST_COMMENTS_DIR ../private_comments &
	# PRIVATE_COMMENTS_DIR=$TEST_COMMENTS_DIR ../private_comments 2> /dev/null &
	disown

	# echo "pid: "$(pgrep private_comments)
	echo "status: "$(curl -s http://localhost:5749/status)
	sleep 0.5
	# sleep because without it the first test starts outputing and THEN
	# we see the output from PC booting
	# makes the output ugly


	PROJECT_NAME_HASH="pc_testing_test_project_hash"
	FILE_PATH_HASH="41c4765834d1627278775e6b5faa920d98d82dddcae6e4bf66b32de9725b1e00"
	TREEISH_ONE="d689d9f4012e2d222cb69bfcb9d0bab2c66f5c7d"

	echo "END SETUP"
}

teardown_suite () {
	# echo "BEGIN TEARDOWN"
	# echo "Shutting down server"
	pkill private_comments
	# echo "END TEARDOWN"
}


