gloom
=====

An OTP framework for creating Multi-User Dungeons

Usage
=====

To use Gloom, include `gloom` in your `.app` file or use `application:start(gloom)`. Then run the function `gloom:start_listener/4`.

A callback module must be supplied `gloom:start_listener/4` which will be used as the initial state for the session.

Callback modules operate in a queue which can be pushed and popped to change the state.
